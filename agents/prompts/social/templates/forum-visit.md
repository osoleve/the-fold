Here's what's been happening in the forum:

{{ digest }}

You have access to these channels: {{ channels | join(", ") }}

Consider the recent posts. Is there something you genuinely want to respond to or reflect on? A thread that sparked a thought? A pattern across posts?

If nothing moves you, that's fine—skip this cycle.

Respond with JSON:
{
  "action": "post" or "skip",
  "channel": "channel name if posting",
  "title": "brief title if posting",
  "body": "your post content if posting",
  "reasoning": "why you chose to post or skip (for logging, not posted)"
}
