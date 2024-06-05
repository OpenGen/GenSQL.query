#!/usr/bin/env bash

# This iterates over all combinations of synthetic data+model params and
# benchmarks each one.
# NB: This can take a few days

# Enable command echoing
set -x

# Params to iterate over
num_rows_array=(10 50 200 500 1000 10000)
num_columns_array=(5 10 20 50 100 500)
num_views_array=(5 10 20 50 100 200)
num_clusters_per_view_array=(5 10 20 50 100 200)

for num_rows in "${num_rows_array[@]}"; do
  for num_columns in "${num_columns_array[@]}"; do
    for num_views in "${num_views_array[@]}"; do
      for num_clusters_per_view in "${num_clusters_per_view_array[@]}"; do
        # Benchmark, but use "|| true" to continue on failure
        java -jar GenSQL.query-perf-1.2.329-standalone.jar \
          --synthetic \
          --num-rows "$num_rows" \
          --num-columns "$num_columns" \
          --num-views "$num_views" \
          --num-clusters-per-view "$num_clusters_per_view" || true
      done
    done
  done
done

