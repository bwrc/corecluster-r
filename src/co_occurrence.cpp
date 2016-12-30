#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List co_occurrence_mat(Rcpp::NumericVector idx, Rcpp::NumericVector cl, Rcpp::NumericMatrix same, Rcpp::NumericMatrix count) {
    // Length of data
    int n = idx.size();

    // Get the unique values
    Rcpp::LogicalVector dup = ! (Rcpp::duplicated(idx));
    Rcpp::IntegerVector uniq = Rcpp::seq_len(n);
    uniq = uniq[dup];

    int n_uniq = uniq.size();
    int i1, i2;

    // Co-occurrence probs
    for (int i = 0; i < n_uniq; i++) {
        for (int j = (i + 1); j < n_uniq; j++) {
            i1 = idx[uniq[i] - 1] - 1;
            i2 = idx[uniq[j] - 1] - 1;

            //Rcpp::Rcout << "-->\t" << i1 << "\t\t" << i2 << std::endl;
            count(i1, i2) += 1.0;
            count(i2, i1) += 1.0;

            if (cl[uniq[i] - 1] == cl[uniq[j] - 1]) {
                same(i1, i2) += 1.0;
                same(i2, i1) += 1.0;
            }
        }
    }

    return Rcpp::List::create(Rcpp::Named("same") = same,
                              Rcpp::Named("count") = count);
}
