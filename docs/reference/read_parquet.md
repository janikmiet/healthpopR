# Read a Parquet file using DuckDB

Reads a Parquet file into R using DuckDB. This function opens a
temporary DuckDB connection, executes a SQL query on the Parquet file,
and returns the result as a data frame. Optional SQL clauses (e.g.,
\`SELECT\`, \`WHERE\`) can be supplied through \`statement_extra\`.

## Usage

``` r
read_parquet(file, statement_extra = "")
```

## Arguments

- file:

  Character. Path to the Parquet file.

- statement_extra:

  Character. Optional SQL statement prefix that is inserted before
  \`FROM read_parquet(...)\`. This can be used to specify \`SELECT\`,
  \`WHERE\`, or other SQL clauses. Default is \`""\`.

## Value

A data.frame containing the query result.

## Details

The function creates a temporary DuckDB connection for each call and
automatically disconnects it after the query is executed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read entire parquet file
df <- read_parquet("data/example.parquet", "SELECT * ")

# Read filtered data
df <- read_parquet(
  "data/example.parquet",
  "SELECT * WHERE age > 50 "
)
} # }
```
