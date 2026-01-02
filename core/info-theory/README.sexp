((name "info-theory")
 (purpose "Information theory toolkit")
 (description "Information-theoretic quantities for discrete and continuous
distributions. Provides Shannon entropy, mutual information, KL divergence,
and related measures for analyzing probability distributions and data.")
 (modules
  ((entropy.ss "Shannon entropy, joint/conditional entropy, mutual information,
   cross entropy, KL divergence, relative entropy")))
 (dependencies (base))
 (key-concepts
  ((shannon-entropy "H(X) = -sum p(x) log p(x)")
   (mutual-information "I(X;Y) = H(X) + H(Y) - H(X,Y)")
   (kl-divergence "D_KL(P||Q) = sum P(x) log(P(x)/Q(x))")))
 (test-coverage "57 tests in test-entropy.ss"))
