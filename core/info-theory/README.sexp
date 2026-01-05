((name "info-theory")
 (purpose "Information theory toolkit")
 (description "Information-theoretic quantities for discrete and continuous
distributions. Provides Shannon entropy, mutual information, KL divergence,
and comprehensive statistical divergence measures for analyzing and comparing
probability distributions.")
 (modules
  ((entropy.ss "Shannon entropy, joint/conditional entropy, mutual information,
   cross entropy, KL divergence, Renyi entropy, differential entropy")
   (statistical-measures.ss "Statistical divergence measures: KL, Jensen-Shannon,
   Bhattacharyya, Hellinger, total variation, chi-squared, Jeffreys, alpha-divergence,
   triangular discrimination, K-divergence, squared loss, Matusita, fidelity")))
 (dependencies (base fp/numeric/transcendental))
 (key-concepts
  ((shannon-entropy "H(X) = -sum p(x) log p(x)")
   (mutual-information "I(X;Y) = H(X) + H(Y) - H(X,Y)")
   (kl-divergence "D_KL(P||Q) = sum P(x) log(P(x)/Q(x))")
   (hellinger-distance "H(P,Q) = sqrt(1/2 * sum((sqrt(p_i) - sqrt(q_i))^2))")
   (jensen-shannon "JSD(P||Q) = (D_KL(P||M) + D_KL(Q||M))/2 where M = (P+Q)/2")
   (bhattacharyya "BC(P,Q) = sum(sqrt(p_i * q_i))")))
 (test-coverage "121 tests: 57 in test-entropy.ss, 64 in test-statistical-measures.ss"))
