#include <Rcpp.h>
#include <random>

//' C++ implementation of R's sample function with probabilities
//' @param x NumericVector - The input vector from R
//' @param size int - The number of elements to sample
//' @param replace bool - Whether sampling is done with replacement
//' @param prob NumericVector - The probabilities of elements to sample
//' @return NumericVector - The sampled elements
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector sample_cpp_prob(Rcpp::NumericVector x, int size, bool replace = false, Rcpp::NumericVector prob=Rcpp::NumericVector()){
  int n = x.size();
  std::vector<int> indices(n);
  
  // Initialize indices
  for (int i = 0; i < n; ++i) {
    indices[i] = i;
  }
  
  // Random number generator
  std::random_device rd;
  std::mt19937 gen(rd());
  
  Rcpp::NumericVector sampled_elements(size);
  
  if (prob.size() == 0 || prob.size() != n) {
    // If probabilities are not provided or the size is not equal to the input vector,
    // use equal probabilities for each element
    for (int i = 0; i < n; ++i) {
      prob.push_back(1.0 / n);
    }
  }
  
  std::discrete_distribution<int> distribution(prob.begin(), prob.end());
  
  if (!replace) {
    // Sampling without replacement
    for (int i = 0; i < size; ++i) {
      int idx = distribution(gen);
      sampled_elements[i] = x[indices[idx]];
      indices.erase(indices.begin() + idx);
      prob.erase(prob.begin() + idx);
    }
  } else {
    // Sampling with replacement
    for (int i = 0; i < size; ++i) {
      int idx = distribution(gen);
      sampled_elements[i] = x[idx];
    }
  }
  
  return sampled_elements;
}
