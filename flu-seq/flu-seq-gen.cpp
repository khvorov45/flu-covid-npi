#include <Rcpp.h>

using namespace Rcpp;

bool find_row_cpp(
    String subtype_value, String country_value,
    Date min_date, Date max_date,
    DataFrame& sequence
) {
    StringVector subtype_vec = sequence["subtype"];
    StringVector country_vec = sequence["country_name"];
    DateVector date_vec = sequence["date"];

    for (int index_row = 0; index_row < sequence.nrows(); index_row++) {

        Date date = date_vec[index_row];

        bool row_is_appropriate =
            subtype_vec[index_row] == subtype_value &&
            country_vec[index_row] == country_value &&
            date >= min_date &&
            date <= max_date;

        if (row_is_appropriate == NA_LOGICAL) {
            continue;
        }
        if (row_is_appropriate) {
            return true;
        }
    }

    return false;
}

// [[Rcpp::export]]
LogicalVector is_accompanied_by_sequence_cpp(
    StringVector& subtype_values, StringVector& country_values,
    DateVector& min_dates, DateVector& max_dates,
    DataFrame& sequence
) {
    LogicalVector result(subtype_values.length());
    for (int index = 0; index < result.length(); index++) {
        result[index] = find_row_cpp(
            subtype_values[index], country_values[index],
            min_dates[index], max_dates[index],
            sequence
        );
    }
    return result;
}
