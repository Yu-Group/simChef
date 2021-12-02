# Functions in Evaluator prediction library work properly

    # A tibble: 12 x 5
        .rep .dgp_name .method_name .metric .estimate
       <int> <chr>     <chr>        <chr>       <dbl>
     1     1 DGP1      Method       rmse         5.82
     2     1 DGP1      Method       rsq          1   
     3     1 DGP1      Method       mae          5.05
     4     2 DGP1      Method       rmse        64.0 
     5     2 DGP1      Method       rsq          1   
     6     2 DGP1      Method       mae         55.6 
     7     1 DGP2      Method       rmse       122.  
     8     1 DGP2      Method       rsq          1   
     9     1 DGP2      Method       mae        106.  
    10     2 DGP2      Method       rmse       180.  
    11     2 DGP2      Method       rsq          1   
    12     2 DGP2      Method       mae        157.  

---

    # A tibble: 6 x 9
    # Groups:   .dgp_name, .method_name, .metric [6]
      .dgp_name .method_name .metric mean_pred_err median_pred_err min_pred_err
      <chr>     <chr>        <chr>           <dbl>           <dbl>        <dbl>
    1 DGP1      Method       mae              30.3            30.3         5.05
    2 DGP1      Method       rmse             34.9            34.9         5.82
    3 DGP1      Method       rsq               1               1           1   
    4 DGP2      Method       mae             131.            131.        106.  
    5 DGP2      Method       rmse            151.            151.        122.  
    6 DGP2      Method       rsq               1               1           1   
    # ... with 3 more variables: max_pred_err <dbl>, sd_pred_err <dbl>,
    #   raw_pred_err <list>

---

    # A tibble: 36 x 6
        .rep .dgp_name .method_name .group .metric .estimate
       <int> <chr>     <chr>        <chr>  <chr>       <dbl>
     1     1 DGP1      Method       .all   rmse         5.82
     2     1 DGP1      Method       a      rmse         5.77
     3     1 DGP1      Method       b      rmse         5.86
     4     1 DGP1      Method       .all   rsq          1   
     5     1 DGP1      Method       a      rsq          1   
     6     1 DGP1      Method       b      rsq          1   
     7     1 DGP1      Method       .all   mae          5.05
     8     1 DGP1      Method       a      mae          5.00
     9     1 DGP1      Method       b      mae          5.10
    10     2 DGP1      Method       .all   rmse        64.0 
    # ... with 26 more rows

---

    # A tibble: 18 x 10
    # Groups:   .dgp_name, .method_name, .group, .metric [18]
       .dgp_name .method_name .group .metric mean_pred_err median_pred_err
       <chr>     <chr>        <chr>  <chr>           <dbl>           <dbl>
     1 DGP1      Method       .all   mae              30.3            30.3
     2 DGP1      Method       .all   rmse             34.9            34.9
     3 DGP1      Method       .all   rsq               1               1  
     4 DGP1      Method       a      mae              30              30  
     5 DGP1      Method       a      rmse             34.6            34.6
     6 DGP1      Method       a      rsq               1               1  
     7 DGP1      Method       b      mae              30.6            30.6
     8 DGP1      Method       b      rmse             35.2            35.2
     9 DGP1      Method       b      rsq               1               1  
    10 DGP2      Method       .all   mae             131.            131. 
    11 DGP2      Method       .all   rmse            151.            151. 
    12 DGP2      Method       .all   rsq               1               1  
    13 DGP2      Method       a      mae             130             130  
    14 DGP2      Method       a      rmse            150.            150. 
    15 DGP2      Method       a      rsq               1               1  
    16 DGP2      Method       b      mae             133.            133. 
    17 DGP2      Method       b      rmse            152.            152. 
    18 DGP2      Method       b      rsq               1               1  
    # ... with 4 more variables: min_pred_err <dbl>, max_pred_err <dbl>,
    #   sd_pred_err <dbl>, raw_pred_err <list>

---

    # A tibble: 8 x 5
       .rep .dgp_name .method_name .metric .estimate
      <int> <chr>     <chr>        <chr>       <dbl>
    1     1 DGP1      Method       rmse         5.82
    2     1 DGP1      Method       rsq          1   
    3     2 DGP1      Method       rmse        64.0 
    4     2 DGP1      Method       rsq          1   
    5     1 DGP2      Method       rmse       122.  
    6     1 DGP2      Method       rsq          1   
    7     2 DGP2      Method       rmse       180.  
    8     2 DGP2      Method       rsq          1   

---

    # A tibble: 4 x 9
    # Groups:   .dgp_name, .method_name, .metric [4]
      .dgp_name .method_name .metric mean_pred_err median_pred_err min_pred_err
      <chr>     <chr>        <chr>           <dbl>           <dbl>        <dbl>
    1 DGP1      Method       rmse             34.9            34.9         5.82
    2 DGP1      Method       rsq               1               1           1   
    3 DGP2      Method       rmse            151.            151.        122.  
    4 DGP2      Method       rsq               1               1           1   
    # ... with 3 more variables: max_pred_err <dbl>, sd_pred_err <dbl>,
    #   raw_pred_err <list>

---

    # A tibble: 6 x 10
    # Groups:   .dgp_name, .method_name, .metric [6]
      .dgp_name .method_name .metric mean_pred_err median_pred_err min_pred_err
      <chr>     <chr>        <chr>           <dbl>           <dbl>        <dbl>
    1 DGP1      Method       mae              30.3            30.3         5.05
    2 DGP1      Method       rmse             34.9            34.9         5.82
    3 DGP1      Method       rsq               1               1           1   
    4 DGP2      Method       mae             131.            131.        106.  
    5 DGP2      Method       rmse            151.            151.        122.  
    6 DGP2      Method       rsq               1               1           1   
    # ... with 4 more variables: max_pred_err <dbl>, sd_pred_err <dbl>,
    #   raw_pred_err <list>, range_pred_err <dbl>

---

    # A tibble: 16 x 5
        .rep .dgp_name .method_name .metric     .estimate
       <int> <chr>     <chr>        <chr>           <dbl>
     1     1 DGP1      Method       accuracy         0.5 
     2     1 DGP1      Method       kap              0   
     3     1 DGP1      Method       mn_log_loss      1.33
     4     1 DGP1      Method       roc_auc          0.49
     5     2 DGP1      Method       accuracy         0.5 
     6     2 DGP1      Method       kap              0   
     7     2 DGP1      Method       mn_log_loss      1.33
     8     2 DGP1      Method       roc_auc          0.49
     9     1 DGP2      Method       accuracy         0.5 
    10     1 DGP2      Method       kap              0   
    11     1 DGP2      Method       mn_log_loss      1.33
    12     1 DGP2      Method       roc_auc          0.49
    13     2 DGP2      Method       accuracy         0.5 
    14     2 DGP2      Method       kap              0   
    15     2 DGP2      Method       mn_log_loss      1.33
    16     2 DGP2      Method       roc_auc          0.49

---

    # A tibble: 8 x 9
    # Groups:   .dgp_name, .method_name, .metric [8]
      .dgp_name .method_name .metric     mean_pred_err median_pred_err min_pred_err
      <chr>     <chr>        <chr>               <dbl>           <dbl>        <dbl>
    1 DGP1      Method       accuracy             0.5             0.5          0.5 
    2 DGP1      Method       kap                  0               0            0   
    3 DGP1      Method       mn_log_loss          1.33            1.33         1.33
    4 DGP1      Method       roc_auc              0.49            0.49         0.49
    5 DGP2      Method       accuracy             0.5             0.5          0.5 
    6 DGP2      Method       kap                  0               0            0   
    7 DGP2      Method       mn_log_loss          1.33            1.33         1.33
    8 DGP2      Method       roc_auc              0.49            0.49         0.49
    # ... with 3 more variables: max_pred_err <dbl>, sd_pred_err <dbl>,
    #   raw_pred_err <list>

---

    # A tibble: 8 x 5
       .rep .dgp_name .method_name .metric  .estimate
      <int> <chr>     <chr>        <chr>        <dbl>
    1     1 DGP1      Method       accuracy       0.5
    2     1 DGP1      Method       kap            0  
    3     2 DGP1      Method       accuracy       0.5
    4     2 DGP1      Method       kap            0  
    5     1 DGP2      Method       accuracy       0.5
    6     1 DGP2      Method       kap            0  
    7     2 DGP2      Method       accuracy       0.5
    8     2 DGP2      Method       kap            0  

---

    # A tibble: 4 x 9
    # Groups:   .dgp_name, .method_name, .metric [4]
      .dgp_name .method_name .metric  mean_pred_err median_pred_err min_pred_err
      <chr>     <chr>        <chr>            <dbl>           <dbl>        <dbl>
    1 DGP1      Method       accuracy           0.5             0.5          0.5
    2 DGP1      Method       kap                0               0            0  
    3 DGP2      Method       accuracy           0.5             0.5          0.5
    4 DGP2      Method       kap                0               0            0  
    # ... with 3 more variables: max_pred_err <dbl>, sd_pred_err <dbl>,
    #   raw_pred_err <list>

---

    # A tibble: 16 x 5
        .rep .dgp_name .method_name .metric     .estimate
       <int> <chr>     <chr>        <chr>           <dbl>
     1     1 DGP1      Method       accuracy        0.333
     2     1 DGP1      Method       kap             0    
     3     1 DGP1      Method       mn_log_loss     1.32 
     4     1 DGP1      Method       roc_auc         0.5  
     5     2 DGP1      Method       accuracy        0.333
     6     2 DGP1      Method       kap             0    
     7     2 DGP1      Method       mn_log_loss     1.32 
     8     2 DGP1      Method       roc_auc         0.5  
     9     1 DGP2      Method       accuracy        0.333
    10     1 DGP2      Method       kap             0    
    11     1 DGP2      Method       mn_log_loss     1.32 
    12     1 DGP2      Method       roc_auc         0.5  
    13     2 DGP2      Method       accuracy        0.333
    14     2 DGP2      Method       kap             0    
    15     2 DGP2      Method       mn_log_loss     1.32 
    16     2 DGP2      Method       roc_auc         0.5  

---

    # A tibble: 8 x 9
    # Groups:   .dgp_name, .method_name, .metric [8]
      .dgp_name .method_name .metric     mean_pred_err median_pred_err min_pred_err
      <chr>     <chr>        <chr>               <dbl>           <dbl>        <dbl>
    1 DGP1      Method       accuracy            0.333           0.333        0.333
    2 DGP1      Method       kap                 0               0            0    
    3 DGP1      Method       mn_log_loss         1.32            1.32         1.32 
    4 DGP1      Method       roc_auc             0.5             0.5          0.5  
    5 DGP2      Method       accuracy            0.333           0.333        0.333
    6 DGP2      Method       kap                 0               0            0    
    7 DGP2      Method       mn_log_loss         1.32            1.32         1.32 
    8 DGP2      Method       roc_auc             0.5             0.5          0.5  
    # ... with 3 more variables: max_pred_err <dbl>, sd_pred_err <dbl>,
    #   raw_pred_err <list>

---

    # A tibble: 8 x 5
       .rep .dgp_name .method_name .metric  .estimate
      <int> <chr>     <chr>        <chr>        <dbl>
    1     1 DGP1      Method       accuracy     0.333
    2     1 DGP1      Method       kap          0    
    3     2 DGP1      Method       accuracy     0.333
    4     2 DGP1      Method       kap          0    
    5     1 DGP2      Method       accuracy     0.333
    6     1 DGP2      Method       kap          0    
    7     2 DGP2      Method       accuracy     0.333
    8     2 DGP2      Method       kap          0    

---

    # A tibble: 4 x 9
    # Groups:   .dgp_name, .method_name, .metric [4]
      .dgp_name .method_name .metric  mean_pred_err median_pred_err min_pred_err
      <chr>     <chr>        <chr>            <dbl>           <dbl>        <dbl>
    1 DGP1      Method       accuracy         0.333           0.333        0.333
    2 DGP1      Method       kap              0               0            0    
    3 DGP2      Method       accuracy         0.333           0.333        0.333
    4 DGP2      Method       kap              0               0            0    
    # ... with 3 more variables: max_pred_err <dbl>, sd_pred_err <dbl>,
    #   raw_pred_err <list>

---

    # A tibble: 4 x 4
       .rep .dgp_name .method_name curve_estimate    
      <int> <chr>     <chr>        <list>            
    1     1 DGP1      Method       <roc_df [102 x 3]>
    2     2 DGP1      Method       <roc_df [102 x 3]>
    3     1 DGP2      Method       <roc_df [102 x 3]>
    4     2 DGP2      Method       <roc_df [102 x 3]>

---

    # A tibble: 4 x 4
       .rep .dgp_name .method_name curve_estimate   
      <int> <chr>     <chr>        <list>           
    1     1 DGP1      Method       <pr_df [101 x 3]>
    2     2 DGP1      Method       <pr_df [101 x 3]>
    3     1 DGP2      Method       <pr_df [101 x 3]>
    4     2 DGP2      Method       <pr_df [101 x 3]>

---

    # A tibble: 202 x 9
    # Groups:   .dgp_name, .method_name, FPR [202]
       .dgp_name .method_name   FPR mean_TPR median_TPR min_TPR max_TPR sd_TPR
       <chr>     <chr>        <dbl>    <dbl>      <dbl>   <dbl>   <dbl>  <dbl>
     1 DGP1      Method        0        0          0       0       0         0
     2 DGP1      Method        0.01     0          0       0       0         0
     3 DGP1      Method        0.02     0          0       0       0         0
     4 DGP1      Method        0.03     0          0       0       0         0
     5 DGP1      Method        0.04     0.02       0.02    0.02    0.02      0
     6 DGP1      Method        0.05     0.02       0.02    0.02    0.02      0
     7 DGP1      Method        0.06     0.04       0.04    0.04    0.04      0
     8 DGP1      Method        0.07     0.04       0.04    0.04    0.04      0
     9 DGP1      Method        0.08     0.06       0.06    0.06    0.06      0
    10 DGP1      Method        0.09     0.08       0.08    0.08    0.08      0
    # ... with 192 more rows, and 1 more variable: raw_TPR <list>

---

    # A tibble: 202 x 9
    # Groups:   .dgp_name, .method_name, recall [202]
       .dgp_name .method_name recall mean_precision median_precision min_precision
       <chr>     <chr>         <dbl>          <dbl>            <dbl>         <dbl>
     1 DGP1      Method         0             1                1             1    
     2 DGP1      Method         0.01          1                1             1    
     3 DGP1      Method         0.02          0.5              0.5           0.5  
     4 DGP1      Method         0.03          0.333            0.333         0.333
     5 DGP1      Method         0.04          0.5              0.5           0.5  
     6 DGP1      Method         0.05          0.4              0.4           0.4  
     7 DGP1      Method         0.06          0.5              0.5           0.5  
     8 DGP1      Method         0.07          0.429            0.429         0.429
     9 DGP1      Method         0.08          0.5              0.5           0.5  
    10 DGP1      Method         0.09          0.444            0.444         0.444
    # ... with 192 more rows, and 3 more variables: max_precision <dbl>,
    #   sd_precision <dbl>, raw_precision <list>

---

    # A tibble: 4 x 4
       .rep .dgp_name .method_name curve_estimate   
      <int> <chr>     <chr>        <list>           
    1     1 DGP1      Method       <roc_df [19 x 4]>
    2     2 DGP1      Method       <roc_df [19 x 4]>
    3     1 DGP2      Method       <roc_df [19 x 4]>
    4     2 DGP2      Method       <roc_df [19 x 4]>

---

    # A tibble: 4 x 4
       .rep .dgp_name .method_name curve_estimate  
      <int> <chr>     <chr>        <list>          
    1     1 DGP1      Method       <pr_df [16 x 4]>
    2     2 DGP1      Method       <pr_df [16 x 4]>
    3     1 DGP2      Method       <pr_df [16 x 4]>
    4     2 DGP2      Method       <pr_df [16 x 4]>

---

    # A tibble: 202 x 9
    # Groups:   .dgp_name, .method_name, FPR [202]
       .dgp_name .method_name   FPR mean_TPR median_TPR min_TPR max_TPR sd_TPR
       <chr>     <chr>        <dbl>    <dbl>      <dbl>   <dbl>   <dbl>  <dbl>
     1 DGP1      Method        0           0          0       0       0      0
     2 DGP1      Method        0.01        0          0       0       0      0
     3 DGP1      Method        0.02        0          0       0       0      0
     4 DGP1      Method        0.03        0          0       0       0      0
     5 DGP1      Method        0.04        0          0       0       0      0
     6 DGP1      Method        0.05        0          0       0       0      0
     7 DGP1      Method        0.06        0          0       0       0      0
     8 DGP1      Method        0.07        0          0       0       0      0
     9 DGP1      Method        0.08        0          0       0       0      0
    10 DGP1      Method        0.09        0          0       0       0      0
    # ... with 192 more rows, and 1 more variable: raw_TPR <list>

---

    # A tibble: 202 x 9
    # Groups:   .dgp_name, .method_name, recall [202]
       .dgp_name .method_name recall mean_precision median_precision min_precision
       <chr>     <chr>         <dbl>          <dbl>            <dbl>         <dbl>
     1 DGP1      Method         0                 1                1             1
     2 DGP1      Method         0.01              1                1             1
     3 DGP1      Method         0.02              1                1             1
     4 DGP1      Method         0.03              1                1             1
     5 DGP1      Method         0.04              1                1             1
     6 DGP1      Method         0.05              1                1             1
     7 DGP1      Method         0.06              1                1             1
     8 DGP1      Method         0.07              1                1             1
     9 DGP1      Method         0.08              1                1             1
    10 DGP1      Method         0.09              1                1             1
    # ... with 192 more rows, and 3 more variables: max_precision <dbl>,
    #   sd_precision <dbl>, raw_precision <list>

# Functions in Evaluator feature selection library work properly

    # A tibble: 36 x 5
        .rep .dgp_name .method_name .metric .estimate
       <int> <chr>     <chr>        <chr>       <dbl>
     1     1 DGP1      Method       tp          1    
     2     1 DGP1      Method       fp          0    
     3     1 DGP1      Method       sens        0.5  
     4     1 DGP1      Method       spec        1    
     5     1 DGP1      Method       ppv         1    
     6     1 DGP1      Method       pos         1    
     7     1 DGP1      Method       neg         2    
     8     1 DGP1      Method       roc_auc     0.5  
     9     1 DGP1      Method       pr_auc      0.792
    10     2 DGP1      Method       tp          1    
    # ... with 26 more rows

---

    # A tibble: 18 x 9
    # Groups:   .dgp_name, .method_name, .metric [18]
       .dgp_name .method_name .metric mean_feature_selection median_feature_selecti~
       <chr>     <chr>        <chr>                    <dbl>                   <dbl>
     1 DGP1      Method       fp                       0                       0    
     2 DGP1      Method       neg                      2                       2    
     3 DGP1      Method       pos                      1                       1    
     4 DGP1      Method       ppv                      1                       1    
     5 DGP1      Method       pr_auc                   0.896                   0.896
     6 DGP1      Method       roc_auc                  0.75                    0.75 
     7 DGP1      Method       sens                     0.5                     0.5  
     8 DGP1      Method       spec                     1                       1    
     9 DGP1      Method       tp                       1                       1    
    10 DGP2      Method       fp                       0                       0    
    11 DGP2      Method       neg                      2                       2    
    12 DGP2      Method       pos                      1                       1    
    13 DGP2      Method       ppv                      1                       1    
    14 DGP2      Method       pr_auc                   1                       1    
    15 DGP2      Method       roc_auc                  1                       1    
    16 DGP2      Method       sens                     0.5                     0.5  
    17 DGP2      Method       spec                     1                       1    
    18 DGP2      Method       tp                       1                       1    
    # ... with 4 more variables: min_feature_selection <dbl>,
    #   max_feature_selection <dbl>, sd_feature_selection <dbl>,
    #   raw_feature_selection <list>

---

    # A tibble: 8 x 5
       .rep .dgp_name .method_name .metric .estimate
      <int> <chr>     <chr>        <chr>       <dbl>
    1     1 DGP1      Method       sens          0.5
    2     1 DGP1      Method       spec          1  
    3     2 DGP1      Method       sens          0.5
    4     2 DGP1      Method       spec          1  
    5     1 DGP2      Method       sens          0.5
    6     1 DGP2      Method       spec          1  
    7     2 DGP2      Method       sens          0.5
    8     2 DGP2      Method       spec          1  

---

    # A tibble: 4 x 9
    # Groups:   .dgp_name, .method_name, .metric [4]
      .dgp_name .method_name .metric mean_feature_selection median_feature_selection
      <chr>     <chr>        <chr>                    <dbl>                    <dbl>
    1 DGP1      Method       sens                       0.5                      0.5
    2 DGP1      Method       spec                       1                        1  
    3 DGP2      Method       sens                       0.5                      0.5
    4 DGP2      Method       spec                       1                        1  
    # ... with 4 more variables: min_feature_selection <dbl>,
    #   max_feature_selection <dbl>, sd_feature_selection <dbl>,
    #   raw_feature_selection <list>

---

    # A tibble: 18 x 10
    # Groups:   .dgp_name, .method_name, .metric [18]
       .dgp_name .method_name .metric mean_feature_selection median_feature_selecti~
       <chr>     <chr>        <chr>                    <dbl>                   <dbl>
     1 DGP1      Method       fp                       0                       0    
     2 DGP1      Method       neg                      2                       2    
     3 DGP1      Method       pos                      1                       1    
     4 DGP1      Method       ppv                      1                       1    
     5 DGP1      Method       pr_auc                   0.896                   0.896
     6 DGP1      Method       roc_auc                  0.75                    0.75 
     7 DGP1      Method       sens                     0.5                     0.5  
     8 DGP1      Method       spec                     1                       1    
     9 DGP1      Method       tp                       1                       1    
    10 DGP2      Method       fp                       0                       0    
    11 DGP2      Method       neg                      2                       2    
    12 DGP2      Method       pos                      1                       1    
    13 DGP2      Method       ppv                      1                       1    
    14 DGP2      Method       pr_auc                   1                       1    
    15 DGP2      Method       roc_auc                  1                       1    
    16 DGP2      Method       sens                     0.5                     0.5  
    17 DGP2      Method       spec                     1                       1    
    18 DGP2      Method       tp                       1                       1    
    # ... with 5 more variables: min_feature_selection <dbl>,
    #   max_feature_selection <dbl>, sd_feature_selection <dbl>,
    #   raw_feature_selection <list>, range_feature_selection <dbl>

---

    # A tibble: 4 x 4
       .rep .dgp_name .method_name curve_estimate  
      <int> <chr>     <chr>        <list>          
    1     1 DGP1      Method       <roc_df [5 x 3]>
    2     2 DGP1      Method       <roc_df [5 x 3]>
    3     1 DGP2      Method       <roc_df [5 x 3]>
    4     2 DGP2      Method       <roc_df [5 x 3]>

---

    # A tibble: 4 x 4
       .rep .dgp_name .method_name curve_estimate 
      <int> <chr>     <chr>        <list>         
    1     1 DGP1      Method       <pr_df [4 x 3]>
    2     2 DGP1      Method       <pr_df [4 x 3]>
    3     1 DGP2      Method       <pr_df [4 x 3]>
    4     2 DGP2      Method       <pr_df [4 x 3]>

---

    # A tibble: 202 x 9
    # Groups:   .dgp_name, .method_name, FPR [202]
       .dgp_name .method_name   FPR mean_TPR median_TPR min_TPR max_TPR sd_TPR
       <chr>     <chr>        <dbl>    <dbl>      <dbl>   <dbl>   <dbl>  <dbl>
     1 DGP1      Method        0        0.75       0.75     0.5       1  0.354
     2 DGP1      Method        0.01     0.75       0.75     0.5       1  0.354
     3 DGP1      Method        0.02     0.75       0.75     0.5       1  0.354
     4 DGP1      Method        0.03     0.75       0.75     0.5       1  0.354
     5 DGP1      Method        0.04     0.75       0.75     0.5       1  0.354
     6 DGP1      Method        0.05     0.75       0.75     0.5       1  0.354
     7 DGP1      Method        0.06     0.75       0.75     0.5       1  0.354
     8 DGP1      Method        0.07     0.75       0.75     0.5       1  0.354
     9 DGP1      Method        0.08     0.75       0.75     0.5       1  0.354
    10 DGP1      Method        0.09     0.75       0.75     0.5       1  0.354
    # ... with 192 more rows, and 1 more variable: raw_TPR <list>

---

    # A tibble: 202 x 9
    # Groups:   .dgp_name, .method_name, recall [202]
       .dgp_name .method_name recall mean_precision median_precision min_precision
       <chr>     <chr>         <dbl>          <dbl>            <dbl>         <dbl>
     1 DGP1      Method         0                 1                1             1
     2 DGP1      Method         0.01              1                1             1
     3 DGP1      Method         0.02              1                1             1
     4 DGP1      Method         0.03              1                1             1
     5 DGP1      Method         0.04              1                1             1
     6 DGP1      Method         0.05              1                1             1
     7 DGP1      Method         0.06              1                1             1
     8 DGP1      Method         0.07              1                1             1
     9 DGP1      Method         0.08              1                1             1
    10 DGP1      Method         0.09              1                1             1
    # ... with 192 more rows, and 3 more variables: max_precision <dbl>,
    #   sd_precision <dbl>, raw_precision <list>

---

    # A tibble: 12 x 5
        .rep .dgp_name .method_name feature  est_importance
       <int> <chr>     <chr>        <chr>             <dbl>
     1     1 DGP1      Method       featureA            1  
     2     1 DGP1      Method       featureB            1.5
     3     1 DGP1      Method       featureC            2.5
     4     2 DGP1      Method       featureA            2  
     5     2 DGP1      Method       featureB            1.5
     6     2 DGP1      Method       featureC            2.5
     7     1 DGP2      Method       featureA            3  
     8     1 DGP2      Method       featureB            1.5
     9     1 DGP2      Method       featureC            2.5
    10     2 DGP2      Method       featureA            4  
    11     2 DGP2      Method       featureB            1.5
    12     2 DGP2      Method       featureC            2.5

---

    # A tibble: 6 x 9
    # Groups:   .dgp_name, .method_name, feature [6]
      .dgp_name .method_name feature  mean_feature_importance median_feature_import~
      <chr>     <chr>        <chr>                      <dbl>                  <dbl>
    1 DGP1      Method       featureA                     1.5                    1.5
    2 DGP1      Method       featureB                     1.5                    1.5
    3 DGP1      Method       featureC                     2.5                    2.5
    4 DGP2      Method       featureA                     3.5                    3.5
    5 DGP2      Method       featureB                     1.5                    1.5
    6 DGP2      Method       featureC                     2.5                    2.5
    # ... with 4 more variables: min_feature_importance <dbl>,
    #   max_feature_importance <dbl>, sd_feature_importance <dbl>,
    #   raw_feature_importance <list>

# Functions in Evaluator inference library work properly

    # A tibble: 36 x 6
        .rep .dgp_name .method_name .alpha .metric .estimate
       <int> <chr>     <chr>         <dbl> <chr>       <dbl>
     1     1 DGP1      Method         0.05 tp          0    
     2     1 DGP1      Method         0.05 fp          1    
     3     1 DGP1      Method         0.05 sens        0    
     4     1 DGP1      Method         0.05 spec        0    
     5     1 DGP1      Method         0.05 ppv         0    
     6     1 DGP1      Method         0.05 pos         1    
     7     1 DGP1      Method         0.05 neg         2    
     8     1 DGP1      Method         0.05 roc_auc     0    
     9     1 DGP1      Method         0.05 pr_auc      0.417
    10     2 DGP1      Method         0.05 tp          0    
    # ... with 26 more rows

---

    # A tibble: 18 x 10
    # Groups:   .dgp_name, .method_name, .metric, .alpha [18]
       .dgp_name .method_name .metric .alpha mean_testing_err median_testing_err
       <chr>     <chr>        <chr>    <dbl>            <dbl>              <dbl>
     1 DGP1      Method       fp        0.05            1                  1    
     2 DGP1      Method       neg       0.05            2                  2    
     3 DGP1      Method       pos       0.05            1                  1    
     4 DGP1      Method       ppv       0.05            0                  0    
     5 DGP1      Method       pr_auc    0.05            0.417              0.417
     6 DGP1      Method       roc_auc   0.05            0                  0    
     7 DGP1      Method       sens      0.05            0                  0    
     8 DGP1      Method       spec      0.05            0                  0    
     9 DGP1      Method       tp        0.05            0                  0    
    10 DGP2      Method       fp        0.05            1                  1    
    11 DGP2      Method       neg       0.05            1                  1    
    12 DGP2      Method       pos       0.05            2                  2    
    13 DGP2      Method       ppv       0.05            0.5                0.5  
    14 DGP2      Method       pr_auc    0.05            0.792              0.792
    15 DGP2      Method       roc_auc   0.05            0.5                0.5  
    16 DGP2      Method       sens      0.05            0.5                0.5  
    17 DGP2      Method       spec      0.05            0                  0    
    18 DGP2      Method       tp        0.05            1                  1    
    # ... with 4 more variables: min_testing_err <dbl>, max_testing_err <dbl>,
    #   sd_testing_err <dbl>, raw_testing_err <list>

---

    # A tibble: 8 x 6
       .rep .dgp_name .method_name .alpha .metric .estimate
      <int> <chr>     <chr>         <dbl> <chr>       <dbl>
    1     1 DGP1      Method         0.05 sens          0  
    2     1 DGP1      Method         0.05 spec          0  
    3     2 DGP1      Method         0.05 sens          0  
    4     2 DGP1      Method         0.05 spec          0  
    5     1 DGP2      Method         0.05 sens          0.5
    6     1 DGP2      Method         0.05 spec          0  
    7     2 DGP2      Method         0.05 sens          0.5
    8     2 DGP2      Method         0.05 spec          0  

---

    # A tibble: 4 x 10
    # Groups:   .dgp_name, .method_name, .metric, .alpha [4]
      .dgp_name .method_name .metric .alpha mean_testing_err median_testing_err
      <chr>     <chr>        <chr>    <dbl>            <dbl>              <dbl>
    1 DGP1      Method       sens      0.05              0                  0  
    2 DGP1      Method       spec      0.05              0                  0  
    3 DGP2      Method       sens      0.05              0.5                0.5
    4 DGP2      Method       spec      0.05              0                  0  
    # ... with 4 more variables: min_testing_err <dbl>, max_testing_err <dbl>,
    #   sd_testing_err <dbl>, raw_testing_err <list>

---

    # A tibble: 72 x 6
        .rep .dgp_name .method_name .alpha .metric .estimate
       <int> <chr>     <chr>         <dbl> <chr>       <dbl>
     1     1 DGP1      Method         0.05 tp          0    
     2     1 DGP1      Method         0.05 fp          1    
     3     1 DGP1      Method         0.05 sens        0    
     4     1 DGP1      Method         0.05 spec        0    
     5     1 DGP1      Method         0.05 ppv         0    
     6     1 DGP1      Method         0.05 pos         1    
     7     1 DGP1      Method         0.05 neg         2    
     8     1 DGP1      Method         0.05 roc_auc     0    
     9     1 DGP1      Method         0.05 pr_auc      0.417
    10     1 DGP1      Method         0.1  tp          0    
    # ... with 62 more rows

---

    # A tibble: 36 x 10
    # Groups:   .dgp_name, .method_name, .metric, .alpha [36]
       .dgp_name .method_name .metric .alpha mean_testing_err median_testing_err
       <chr>     <chr>        <chr>    <dbl>            <dbl>              <dbl>
     1 DGP1      Method       fp        0.05            1                  1    
     2 DGP1      Method       fp        0.1             1                  1    
     3 DGP1      Method       neg       0.05            2                  2    
     4 DGP1      Method       neg       0.1             1.5                1.5  
     5 DGP1      Method       pos       0.05            1                  1    
     6 DGP1      Method       pos       0.1             1.5                1.5  
     7 DGP1      Method       ppv       0.05            0                  0    
     8 DGP1      Method       ppv       0.1             0.25               0.25 
     9 DGP1      Method       pr_auc    0.05            0.417              0.417
    10 DGP1      Method       pr_auc    0.1             0.417              0.417
    # ... with 26 more rows, and 4 more variables: min_testing_err <dbl>,
    #   max_testing_err <dbl>, sd_testing_err <dbl>, raw_testing_err <list>

---

    # A tibble: 18 x 11
    # Groups:   .dgp_name, .method_name, .metric, .alpha [18]
       .dgp_name .method_name .metric .alpha mean_testing_err median_testing_err
       <chr>     <chr>        <chr>    <dbl>            <dbl>              <dbl>
     1 DGP1      Method       fp        0.05            1                  1    
     2 DGP1      Method       neg       0.05            2                  2    
     3 DGP1      Method       pos       0.05            1                  1    
     4 DGP1      Method       ppv       0.05            0                  0    
     5 DGP1      Method       pr_auc    0.05            0.417              0.417
     6 DGP1      Method       roc_auc   0.05            0                  0    
     7 DGP1      Method       sens      0.05            0                  0    
     8 DGP1      Method       spec      0.05            0                  0    
     9 DGP1      Method       tp        0.05            0                  0    
    10 DGP2      Method       fp        0.05            1                  1    
    11 DGP2      Method       neg       0.05            1                  1    
    12 DGP2      Method       pos       0.05            2                  2    
    13 DGP2      Method       ppv       0.05            0.5                0.5  
    14 DGP2      Method       pr_auc    0.05            0.792              0.792
    15 DGP2      Method       roc_auc   0.05            0.5                0.5  
    16 DGP2      Method       sens      0.05            0.5                0.5  
    17 DGP2      Method       spec      0.05            0                  0    
    18 DGP2      Method       tp        0.05            1                  1    
    # ... with 5 more variables: min_testing_err <dbl>, max_testing_err <dbl>,
    #   sd_testing_err <dbl>, raw_testing_err <list>, range_testing_err <dbl>

---

    # A tibble: 4 x 4
       .rep .dgp_name .method_name curve_estimate  
      <int> <chr>     <chr>        <list>          
    1     1 DGP1      Method       <roc_df [5 x 3]>
    2     2 DGP1      Method       <roc_df [5 x 3]>
    3     1 DGP2      Method       <roc_df [5 x 3]>
    4     2 DGP2      Method       <roc_df [5 x 3]>

---

    # A tibble: 4 x 4
       .rep .dgp_name .method_name curve_estimate 
      <int> <chr>     <chr>        <list>         
    1     1 DGP1      Method       <pr_df [4 x 3]>
    2     2 DGP1      Method       <pr_df [4 x 3]>
    3     1 DGP2      Method       <pr_df [4 x 3]>
    4     2 DGP2      Method       <pr_df [4 x 3]>

---

    # A tibble: 202 x 9
    # Groups:   .dgp_name, .method_name, FPR [202]
       .dgp_name .method_name   FPR mean_TPR median_TPR min_TPR max_TPR sd_TPR
       <chr>     <chr>        <dbl>    <dbl>      <dbl>   <dbl>   <dbl>  <dbl>
     1 DGP1      Method        0           0          0       0       0      0
     2 DGP1      Method        0.01        0          0       0       0      0
     3 DGP1      Method        0.02        0          0       0       0      0
     4 DGP1      Method        0.03        0          0       0       0      0
     5 DGP1      Method        0.04        0          0       0       0      0
     6 DGP1      Method        0.05        0          0       0       0      0
     7 DGP1      Method        0.06        0          0       0       0      0
     8 DGP1      Method        0.07        0          0       0       0      0
     9 DGP1      Method        0.08        0          0       0       0      0
    10 DGP1      Method        0.09        0          0       0       0      0
    # ... with 192 more rows, and 1 more variable: raw_TPR <list>

---

    # A tibble: 202 x 9
    # Groups:   .dgp_name, .method_name, recall [202]
       .dgp_name .method_name recall mean_precision median_precision min_precision
       <chr>     <chr>         <dbl>          <dbl>            <dbl>         <dbl>
     1 DGP1      Method         0                 1                1             1
     2 DGP1      Method         0.01              1                1             1
     3 DGP1      Method         0.02              1                1             1
     4 DGP1      Method         0.03              1                1             1
     5 DGP1      Method         0.04              1                1             1
     6 DGP1      Method         0.05              1                1             1
     7 DGP1      Method         0.06              1                1             1
     8 DGP1      Method         0.07              1                1             1
     9 DGP1      Method         0.08              1                1             1
    10 DGP1      Method         0.09              1                1             1
    # ... with 192 more rows, and 3 more variables: max_precision <dbl>,
    #   sd_precision <dbl>, raw_precision <list>

---

    # A tibble: 19 x 5
    # Groups:   .dgp_name, .method_name, feature [6]
       .dgp_name .method_name feature    .alpha reject_prob
       <chr>     <chr>        <chr>       <dbl>       <dbl>
     1 DGP1      Method       featureA   0              0  
     2 DGP1      Method       featureA   0.1            0.5
     3 DGP1      Method       featureA   1              1  
     4 DGP1      Method       featureB   0              0  
     5 DGP1      Method       featureB   0.0316         1  
     6 DGP1      Method       featureB   1              1  
     7 DGP1      Method       featureC   0              0  
     8 DGP1      Method       featureC   1              0  
     9 DGP1      Method       featureC 316.             1  
    10 DGP2      Method       featureA   0              0  
    11 DGP2      Method       featureA   0.001          0.5
    12 DGP2      Method       featureA   0.01           1  
    13 DGP2      Method       featureA   1              1  
    14 DGP2      Method       featureB   0              0  
    15 DGP2      Method       featureB   0.0316         1  
    16 DGP2      Method       featureB   1              1  
    17 DGP2      Method       featureC   0              0  
    18 DGP2      Method       featureC   1              0  
    19 DGP2      Method       featureC 316.             1  

---

    # A tibble: 12 x 5
    # Groups:   .dgp_name, .method_name, feature [6]
       .dgp_name .method_name feature  .alpha reject_prob
       <chr>     <chr>        <chr>     <dbl>       <dbl>
     1 DGP1      Method       featureA   0.05         0  
     2 DGP1      Method       featureA   0.1          0.5
     3 DGP1      Method       featureB   0.05         1  
     4 DGP1      Method       featureB   0.1          1  
     5 DGP1      Method       featureC   0.05         0  
     6 DGP1      Method       featureC   0.1          0  
     7 DGP2      Method       featureA   0.05         1  
     8 DGP2      Method       featureA   0.1          1  
     9 DGP2      Method       featureB   0.05         1  
    10 DGP2      Method       featureB   0.1          1  
    11 DGP2      Method       featureC   0.05         0  
    12 DGP2      Method       featureC   0.1          0  

# Functions in Evaluator utilities library work properly

    # A tibble: 2 x 8
    # Groups:   .dgp_name, .method_name [2]
      .dgp_name .method_name mean_res median_res min_res max_res sd_res raw_res  
      <chr>     <chr>           <dbl>      <dbl>   <int>   <int>  <dbl> <list>   
    1 DGP1      Method            1.5        1.5       1       2  0.707 <int [2]>
    2 DGP2      Method            3.5        3.5       3       4  0.707 <int [2]>

---

    # A tibble: 2 x 4
    # Groups:   .dgp_name, .method_name [2]
      .dgp_name .method_name mean_res sd_res
      <chr>     <chr>           <dbl>  <dbl>
    1 DGP1      Method            1.5  0.707
    2 DGP2      Method            3.5  0.707

---

    # A tibble: 2 x 9
    # Groups:   .dgp_name, .method_name [2]
      .dgp_name .method_name  mean median   min   max    sd raw       range
      <chr>     <chr>        <dbl>  <dbl> <int> <int> <dbl> <list>    <int>
    1 DGP1      Method         1.5    1.5     1     2 0.707 <int [2]>     1
    2 DGP2      Method         3.5    3.5     3     4 0.707 <int [2]>     1

