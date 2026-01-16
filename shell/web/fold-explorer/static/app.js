// The Fold - Block Explorer Application

const Explorer = {
    // State
    currentPage: 0,
    pageSize: 100,
    tagFilter: '',
    graphNodes: [],
    graphEdges: [],
    selectedNode: null,

    // Canvas state for graph
    canvas: null,
    ctx: null,
    dragging: false,
    dragStart: { x: 0, y: 0 },
    offset: { x: 0, y: 0 },
    scale: 1,

    // Initialize the application
    async init() {
        this.setupNavigation();
        this.setupSearch();
        this.setupPagination();
        this.setupGraph();
        await this.loadStats();
        await this.loadBlocks();
    },

    // Set up navigation buttons
    setupNavigation() {
        document.querySelectorAll('.nav-btn').forEach(btn => {
            btn.addEventListener('click', () => {
                // Update button states
                document.querySelectorAll('.nav-btn').forEach(b => b.classList.remove('active'));
                btn.classList.add('active');

                // Show corresponding view
                const view = btn.dataset.view;
                document.querySelectorAll('.view').forEach(v => v.classList.remove('active'));
                document.getElementById(`${view}-view`).classList.add('active');

                // Load data for the view
                if (view === 'heads') this.loadHeads();
            });
        });
    },

    // Set up search functionality
    setupSearch() {
        const searchInput = document.getElementById('search');
        const searchBtn = document.getElementById('search-btn');

        const doSearch = async () => {
            const query = searchInput.value.trim();
            if (!query) return;

            const res = await fetch(`/api/search?q=${encodeURIComponent(query)}&limit=100`);
            const results = await res.json();
            this.renderBlocksTable(results);
        };

        searchBtn.addEventListener('click', doSearch);
        searchInput.addEventListener('keypress', e => {
            if (e.key === 'Enter') doSearch();
        });
    },

    // Set up pagination
    setupPagination() {
        const prevBtn = document.getElementById('prev-page');
        const nextBtn = document.getElementById('next-page');
        const tagFilter = document.getElementById('tag-filter');

        prevBtn.addEventListener('click', () => {
            if (this.currentPage > 0) {
                this.currentPage--;
                this.loadBlocks();
            }
        });

        nextBtn.addEventListener('click', () => {
            this.currentPage++;
            this.loadBlocks();
        });

        tagFilter.addEventListener('change', () => {
            this.tagFilter = tagFilter.value;
            this.currentPage = 0;
            this.loadBlocks();
        });
    },

    // Set up graph canvas
    setupGraph() {
        this.canvas = document.getElementById('graph-canvas');
        this.ctx = this.canvas.getContext('2d');

        // Handle resize
        const resize = () => {
            const rect = this.canvas.parentElement.getBoundingClientRect();
            this.canvas.width = rect.width;
            this.canvas.height = rect.height - 50;
            this.renderGraph();
        };
        window.addEventListener('resize', resize);
        resize();

        // Mouse interactions for pan/zoom
        this.canvas.addEventListener('mousedown', e => {
            this.dragging = true;
            this.dragStart = { x: e.clientX - this.offset.x, y: e.clientY - this.offset.y };
        });

        this.canvas.addEventListener('mousemove', e => {
            if (this.dragging) {
                this.offset.x = e.clientX - this.dragStart.x;
                this.offset.y = e.clientY - this.dragStart.y;
                this.renderGraph();
            }
        });

        this.canvas.addEventListener('mouseup', () => {
            this.dragging = false;
        });

        this.canvas.addEventListener('wheel', e => {
            e.preventDefault();
            const delta = e.deltaY > 0 ? 0.9 : 1.1;
            this.scale = Math.max(0.1, Math.min(5, this.scale * delta));
            this.renderGraph();
        });

        this.canvas.addEventListener('click', e => {
            const rect = this.canvas.getBoundingClientRect();
            const x = (e.clientX - rect.left - this.offset.x) / this.scale;
            const y = (e.clientY - rect.top - this.offset.y) / this.scale;

            // Find clicked node
            for (const node of this.graphNodes) {
                const dx = node.x - x;
                const dy = node.y - y;
                if (Math.sqrt(dx * dx + dy * dy) < 20) {
                    this.selectNode(node);
                    break;
                }
            }
        });

        // Graph controls
        document.getElementById('graph-load').addEventListener('click', () => {
            const hash = document.getElementById('graph-root').value.trim();
            const depth = parseInt(document.getElementById('graph-depth').value) || 2;
            if (hash) this.loadSubgraph(hash, depth);
        });

        document.getElementById('graph-orphans').addEventListener('click', () => this.loadOrphans());
        document.getElementById('graph-popular').addEventListener('click', () => this.loadPopular());
    },

    // Load store statistics
    async loadStats() {
        try {
            const res = await fetch('/api/graph/stats');
            const stats = await res.json();

            document.getElementById('stat-blocks').textContent = stats.total_blocks.toLocaleString();
            document.getElementById('stat-pinned').textContent = stats.total_pinned.toLocaleString();
            document.getElementById('stat-tags').textContent = stats.tags.length;

            // Populate tag list and filter
            const tagList = document.getElementById('tag-list');
            const tagFilter = document.getElementById('tag-filter');
            tagList.innerHTML = '';

            stats.tags.slice(0, 20).forEach(({ tag, count }) => {
                // Tag list item
                const item = document.createElement('div');
                item.className = 'tag-item';
                item.innerHTML = `<span>${tag}</span><span class="tag-count">${count}</span>`;
                item.addEventListener('click', () => {
                    this.tagFilter = tag;
                    tagFilter.value = tag;
                    this.currentPage = 0;
                    this.loadBlocks();
                });
                tagList.appendChild(item);

                // Filter option
                const option = document.createElement('option');
                option.value = tag;
                option.textContent = `${tag} (${count})`;
                tagFilter.appendChild(option);
            });
        } catch (e) {
            console.error('Failed to load stats:', e);
        }
    },

    // Load blocks list
    async loadBlocks() {
        try {
            const offset = this.currentPage * this.pageSize;
            let url = `/api/blocks?offset=${offset}&limit=${this.pageSize}`;
            if (this.tagFilter) url += `&tag=${encodeURIComponent(this.tagFilter)}`;

            const res = await fetch(url);
            const blocks = await res.json();

            this.renderBlocksTable(blocks);

            // Update pagination
            document.getElementById('prev-page').disabled = this.currentPage === 0;
            document.getElementById('next-page').disabled = blocks.length < this.pageSize;
            document.getElementById('page-info').textContent = `Page ${this.currentPage + 1}`;
        } catch (e) {
            console.error('Failed to load blocks:', e);
        }
    },

    // Render blocks table
    renderBlocksTable(blocks) {
        const tbody = document.querySelector('#blocks-table tbody');
        tbody.innerHTML = blocks.map(b => `
            <tr data-hash="${b.hash}">
                <td><code>${b.hash.slice(0, 16)}...</code></td>
                <td>${this.escapeHtml(b.tag)}</td>
                <td>${b.payload_size.toLocaleString()}</td>
                <td>${b.refs_count}</td>
                <td>${b.pinned ? '<span class="pinned-badge">PINNED</span>' : ''}</td>
            </tr>
        `).join('');

        // Add click handlers
        tbody.querySelectorAll('tr').forEach(row => {
            row.addEventListener('click', () => this.loadBlockDetail(row.dataset.hash));
        });
    },

    // Load block detail
    async loadBlockDetail(hash) {
        try {
            const res = await fetch(`/api/blocks/${hash}`);
            const block = await res.json();

            const refsRes = await fetch(`/api/blocks/${hash}/refs`);
            const refs = await refsRes.json();

            const detail = document.getElementById('detail-content');
            detail.innerHTML = `
                <div class="detail-field">
                    <label>Hash</label>
                    <div class="value hash">${block.hash}</div>
                </div>
                <div class="detail-field">
                    <label>Tag</label>
                    <div class="value">${this.escapeHtml(block.tag)}</div>
                </div>
                <div class="detail-field">
                    <label>Payload Size</label>
                    <div class="value">${block.payload_size.toLocaleString()} bytes</div>
                </div>
                <div class="detail-field">
                    <label>Pinned</label>
                    <div class="value">${block.pinned ? 'Yes' : 'No'}</div>
                </div>
                <div class="detail-field">
                    <label>Payload Preview</label>
                    <pre>${this.escapeHtml(block.payload_preview || '')}</pre>
                </div>
                <div class="detail-field">
                    <label>References (${refs.length})</label>
                    <ul class="refs-list">
                        ${refs.map(r => `
                            <li data-hash="${r.hash}">
                                ${r.hash.slice(0, 24)}...
                                <span class="ref-tag">${this.escapeHtml(r.tag || 'unknown')}</span>
                            </li>
                        `).join('')}
                    </ul>
                </div>
            `;

            // Add click handlers for refs
            detail.querySelectorAll('.refs-list li').forEach(li => {
                li.addEventListener('click', () => this.loadBlockDetail(li.dataset.hash));
            });
        } catch (e) {
            console.error('Failed to load block detail:', e);
        }
    },

    // Load heads list
    async loadHeads() {
        try {
            const res = await fetch('/api/heads');
            const heads = await res.json();

            const tbody = document.querySelector('#heads-table tbody');
            tbody.innerHTML = heads.map(h => `
                <tr data-hash="${h.hash}">
                    <td>${this.escapeHtml(h.name)}</td>
                    <td><code>${h.hash.slice(0, 32)}...</code></td>
                </tr>
            `).join('');

            tbody.querySelectorAll('tr').forEach(row => {
                row.addEventListener('click', () => this.loadBlockDetail(row.dataset.hash));
            });
        } catch (e) {
            console.error('Failed to load heads:', e);
        }
    },

    // Load subgraph for visualization
    async loadSubgraph(hash, depth) {
        try {
            const res = await fetch(`/api/graph/subgraph/${hash}?depth=${depth}`);
            const graph = await res.json();
            this.setGraphData(graph.nodes, graph.edges);
        } catch (e) {
            console.error('Failed to load subgraph:', e);
        }
    },

    // Load orphan blocks
    async loadOrphans() {
        try {
            const res = await fetch('/api/graph/orphans?limit=50');
            const orphans = await res.json();

            // Create a simple layout
            const nodes = orphans.map((b, i) => ({
                ...b,
                id: b.hash,
                x: 100 + (i % 10) * 80,
                y: 100 + Math.floor(i / 10) * 80
            }));
            this.setGraphData(nodes, []);
        } catch (e) {
            console.error('Failed to load orphans:', e);
        }
    },

    // Load popular blocks
    async loadPopular() {
        try {
            const res = await fetch('/api/graph/popular?limit=20');
            const popular = await res.json();

            // Create a radial layout
            const centerX = this.canvas.width / 2;
            const centerY = this.canvas.height / 2;
            const nodes = popular.map((b, i) => {
                const angle = (i / popular.length) * Math.PI * 2;
                const radius = 150 + b.inbound_refs * 2;
                return {
                    ...b,
                    id: b.hash,
                    x: centerX + Math.cos(angle) * radius,
                    y: centerY + Math.sin(angle) * radius
                };
            });
            this.setGraphData(nodes, []);
        } catch (e) {
            console.error('Failed to load popular:', e);
        }
    },

    // Set graph data and layout
    setGraphData(nodes, edges) {
        // Apply force-directed layout
        this.graphNodes = nodes.map((n, i) => ({
            ...n,
            x: n.x || this.canvas.width / 2 + (Math.random() - 0.5) * 300,
            y: n.y || this.canvas.height / 2 + (Math.random() - 0.5) * 300,
            vx: 0,
            vy: 0
        }));

        // Build edge lookup
        const nodeMap = new Map(this.graphNodes.map(n => [n.id, n]));
        this.graphEdges = edges.map(e => ({
            source: nodeMap.get(e.source),
            target: nodeMap.get(e.target)
        })).filter(e => e.source && e.target);

        // Run force simulation
        this.runForceSimulation();
    },

    // Simple force-directed layout
    runForceSimulation() {
        const iterations = 100;
        const repulsion = 5000;
        const attraction = 0.01;
        const damping = 0.9;

        for (let i = 0; i < iterations; i++) {
            // Repulsion between all nodes
            for (let j = 0; j < this.graphNodes.length; j++) {
                for (let k = j + 1; k < this.graphNodes.length; k++) {
                    const n1 = this.graphNodes[j];
                    const n2 = this.graphNodes[k];
                    const dx = n2.x - n1.x;
                    const dy = n2.y - n1.y;
                    const dist = Math.sqrt(dx * dx + dy * dy) || 1;
                    const force = repulsion / (dist * dist);
                    const fx = (dx / dist) * force;
                    const fy = (dy / dist) * force;
                    n1.vx -= fx;
                    n1.vy -= fy;
                    n2.vx += fx;
                    n2.vy += fy;
                }
            }

            // Attraction along edges
            for (const edge of this.graphEdges) {
                const dx = edge.target.x - edge.source.x;
                const dy = edge.target.y - edge.source.y;
                const dist = Math.sqrt(dx * dx + dy * dy) || 1;
                const force = dist * attraction;
                const fx = (dx / dist) * force;
                const fy = (dy / dist) * force;
                edge.source.vx += fx;
                edge.source.vy += fy;
                edge.target.vx -= fx;
                edge.target.vy -= fy;
            }

            // Apply velocities with damping
            for (const node of this.graphNodes) {
                node.x += node.vx;
                node.y += node.vy;
                node.vx *= damping;
                node.vy *= damping;
            }
        }

        // Center the graph
        if (this.graphNodes.length > 0) {
            const bounds = this.getGraphBounds();
            const centerX = (bounds.minX + bounds.maxX) / 2;
            const centerY = (bounds.minY + bounds.maxY) / 2;
            const canvasCenterX = this.canvas.width / 2;
            const canvasCenterY = this.canvas.height / 2;
            this.offset.x = canvasCenterX - centerX * this.scale;
            this.offset.y = canvasCenterY - centerY * this.scale;
        }

        this.renderGraph();
    },

    // Get graph bounds
    getGraphBounds() {
        let minX = Infinity, minY = Infinity;
        let maxX = -Infinity, maxY = -Infinity;
        for (const node of this.graphNodes) {
            minX = Math.min(minX, node.x);
            minY = Math.min(minY, node.y);
            maxX = Math.max(maxX, node.x);
            maxY = Math.max(maxY, node.y);
        }
        return { minX, minY, maxX, maxY };
    },

    // Render the graph on canvas
    renderGraph() {
        const ctx = this.ctx;
        const w = this.canvas.width;
        const h = this.canvas.height;

        // Clear
        ctx.fillStyle = '#16213e';
        ctx.fillRect(0, 0, w, h);

        ctx.save();
        ctx.translate(this.offset.x, this.offset.y);
        ctx.scale(this.scale, this.scale);

        // Draw edges
        ctx.strokeStyle = '#333';
        ctx.lineWidth = 1;
        for (const edge of this.graphEdges) {
            ctx.beginPath();
            ctx.moveTo(edge.source.x, edge.source.y);
            ctx.lineTo(edge.target.x, edge.target.y);
            ctx.stroke();
        }

        // Draw nodes
        for (const node of this.graphNodes) {
            const isSelected = this.selectedNode && this.selectedNode.id === node.id;
            const radius = isSelected ? 15 : 10;

            // Node circle
            ctx.beginPath();
            ctx.arc(node.x, node.y, radius, 0, Math.PI * 2);
            ctx.fillStyle = node.pinned ? '#4ade80' : (isSelected ? '#ff6b6b' : '#e94560');
            ctx.fill();

            // Label
            ctx.fillStyle = '#eee';
            ctx.font = '10px sans-serif';
            ctx.textAlign = 'center';
            const label = node.tag || node.id.slice(0, 8);
            ctx.fillText(label, node.x, node.y + radius + 12);
        }

        ctx.restore();

        // Draw instructions
        if (this.graphNodes.length === 0) {
            ctx.fillStyle = '#666';
            ctx.font = '14px sans-serif';
            ctx.textAlign = 'center';
            ctx.fillText('Enter a block hash and click "Load Graph" to visualize', w / 2, h / 2 - 10);
            ctx.fillText('Or click "Show Orphans" or "Show Popular"', w / 2, h / 2 + 10);
        }
    },

    // Select a node in the graph
    selectNode(node) {
        this.selectedNode = node;
        this.renderGraph();
        this.loadBlockDetail(node.id);
    },

    // Escape HTML entities
    escapeHtml(str) {
        if (!str) return '';
        return str
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&#039;');
    }
};

// Initialize when DOM is ready
document.addEventListener('DOMContentLoaded', () => Explorer.init());
