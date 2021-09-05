# Printing Experiment works properly

    Experiment Name: test-print 
       Saved results at: results/test-print 
       DGPs:  
       Methods:  
       Evaluators:  
       Plotters:  
       Vary Across: None

---

    Experiment Name: test-print 
       Saved results at: results/test-print 
       DGPs: DGP1, DGP2 
       Methods: Method1 
       Evaluators: Evaluator1, Evaluator2, Evaluator3 
       Plotters:  
       Vary Across: None

---

    Experiment Name: test-print 
       Saved results at: results/test-print 
       DGPs: DGP1, DGP2 
       Methods: Method1 
       Evaluators: Evaluator1, Evaluator2, Evaluator3 
       Plotters: Plotter1 
       Vary Across: None

---

    Experiment Name: test-print 
       Saved results at: results/test-print 
       DGPs: DGP1, DGP2 
       Methods: Method1 
       Evaluators: Evaluator1, Evaluator2, Evaluator3 
       Plotters: Plotter1 
       Vary Across: 
          DGP: DGP1 
             x:  int [1:3] 1 2 3

---

    Experiment Name: test-print 
       Saved results at: results/test-print 
       DGPs: DGP1, DGP2 
       Methods: Method1 
       Evaluators: Evaluator1, Evaluator2, Evaluator3 
       Plotters: Plotter1 
       Vary Across: 
          Method: Method1 
             x:  int [1:3] 1 2 3

---

    Experiment Name: test-print 
       Saved results at: results/test-print 
       DGPs: DGP1, DGP2 
       Methods: Method1 
       Evaluators: Evaluator1, Evaluator2, Evaluator3 
       Plotters: Plotter1 
       Vary Across: 
          DGP: DGP1 
             x:  int [1:3] 1 2 3
          DGP: DGP2 
             x:  int [1:3] 2 3 4
          Method: Method1 
             x:  int [1:3] 1 2 3

---

    Experiment Name: test-print 
       Saved results at: results/test-print 
       DGPs: DGP1, DGP2 
       Methods: Method1 
       Evaluators: Evaluator1, Evaluator2, Evaluator3 
       Plotters: Plotter1 
       Vary Across: 
          DGP: DGP1 
             x: List of 1
               $ : int [1:3] 1 2 3
          DGP: DGP2 
             x: List of 1
               $ : int [1:3] 2 3 4
          Method: Method1 
             x:  int [1:3] 1 2 3

# Generate data from Experiment works properly

    $DGP1
    $DGP1[[1]]
    $DGP1[[1]][[1]]
    $DGP1[[1]][[1]][[1]]
    [1] 2
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    
    $DGP1[[2]]
    $DGP1[[2]][[1]]
    $DGP1[[2]][[1]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    
    
    $DGP2
    $DGP2[[1]]
    $DGP2[[1]][[1]]
    $DGP2[[1]][[1]][[1]]
    [1] 3
    
    
    attr(,"params")
    named list()
    
    

---

    $DGP1
    $DGP1[[1]]
    $DGP1[[1]][[1]]
    $DGP1[[1]][[1]][[1]]
    [1] 2
    
    
    $DGP1[[1]][[2]]
    $DGP1[[1]][[2]][[1]]
    [1] 2
    
    
    $DGP1[[1]][[3]]
    $DGP1[[1]][[3]][[1]]
    [1] 2
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    
    $DGP1[[2]]
    $DGP1[[2]][[1]]
    $DGP1[[2]][[1]][[1]]
    [1] 3
    
    
    $DGP1[[2]][[2]]
    $DGP1[[2]][[2]][[1]]
    [1] 3
    
    
    $DGP1[[2]][[3]]
    $DGP1[[2]][[3]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    
    
    $DGP2
    $DGP2[[1]]
    $DGP2[[1]][[1]]
    $DGP2[[1]][[1]][[1]]
    [1] 3
    
    
    $DGP2[[1]][[2]]
    $DGP2[[1]][[2]][[1]]
    [1] 3
    
    
    $DGP2[[1]][[3]]
    $DGP2[[1]][[3]][[1]]
    [1] 3
    
    
    attr(,"params")
    named list()
    
    

---

    $DGP1
    $DGP1[[1]]
    $DGP1[[1]][[1]]
    $DGP1[[1]][[1]][[1]]
    [1] 2
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    
    $DGP1[[2]]
    $DGP1[[2]][[1]]
    $DGP1[[2]][[1]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    
    
    $DGP2
    $DGP2[[1]]
    $DGP2[[1]][[1]]
    $DGP2[[1]][[1]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    
    $DGP2[[2]]
    $DGP2[[2]][[1]]
    $DGP2[[2]][[1]][[1]]
    [1] 4
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    
    $DGP2[[3]]
    $DGP2[[3]][[1]]
    $DGP2[[3]][[1]][[1]]
    [1] 5
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 3
    
    
    

---

    $DGP1
    $DGP1[[1]]
    $DGP1[[1]][[1]]
    $DGP1[[1]][[1]][[1]]
    [1] 2
    
    
    $DGP1[[1]][[2]]
    $DGP1[[1]][[2]][[1]]
    [1] 2
    
    
    $DGP1[[1]][[3]]
    $DGP1[[1]][[3]][[1]]
    [1] 2
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    
    $DGP1[[2]]
    $DGP1[[2]][[1]]
    $DGP1[[2]][[1]][[1]]
    [1] 3
    
    
    $DGP1[[2]][[2]]
    $DGP1[[2]][[2]][[1]]
    [1] 3
    
    
    $DGP1[[2]][[3]]
    $DGP1[[2]][[3]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    
    
    $DGP2
    $DGP2[[1]]
    $DGP2[[1]][[1]]
    $DGP2[[1]][[1]][[1]]
    [1] 3
    
    
    $DGP2[[1]][[2]]
    $DGP2[[1]][[2]][[1]]
    [1] 3
    
    
    $DGP2[[1]][[3]]
    $DGP2[[1]][[3]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    
    $DGP2[[2]]
    $DGP2[[2]][[1]]
    $DGP2[[2]][[1]][[1]]
    [1] 4
    
    
    $DGP2[[2]][[2]]
    $DGP2[[2]][[2]][[1]]
    [1] 4
    
    
    $DGP2[[2]][[3]]
    $DGP2[[2]][[3]][[1]]
    [1] 4
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    
    $DGP2[[3]]
    $DGP2[[3]][[1]]
    $DGP2[[3]][[1]][[1]]
    [1] 5
    
    
    $DGP2[[3]][[2]]
    $DGP2[[3]][[2]][[1]]
    [1] 5
    
    
    $DGP2[[3]][[3]]
    $DGP2[[3]][[3]][[1]]
    [1] 5
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 3
    
    
    

---

    $DGP1
    $DGP1[[1]]
    $DGP1[[1]][[1]]
    $DGP1[[1]][[1]][[1]]
    [1] 2
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    attr(,"params")$y
    [1] 1
    
    
    $DGP1[[2]]
    $DGP1[[2]][[1]]
    $DGP1[[2]][[1]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    attr(,"params")$y
    [1] 1
    
    
    $DGP1[[3]]
    $DGP1[[3]][[1]]
    $DGP1[[3]][[1]][[1]]
    [1] 2
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    attr(,"params")$y
    [1] 2
    
    
    $DGP1[[4]]
    $DGP1[[4]][[1]]
    $DGP1[[4]][[1]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    attr(,"params")$y
    [1] 2
    
    
    
    $DGP2
    $DGP2[[1]]
    $DGP2[[1]][[1]]
    $DGP2[[1]][[1]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    
    $DGP2[[2]]
    $DGP2[[2]][[1]]
    $DGP2[[2]][[1]][[1]]
    [1] 4
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    
    $DGP2[[3]]
    $DGP2[[3]][[1]]
    $DGP2[[3]][[1]][[1]]
    [1] 5
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 3
    
    
    

---

    $DGP1
    $DGP1[[1]]
    $DGP1[[1]][[1]]
    $DGP1[[1]][[1]][[1]]
    [1] 2
    
    
    $DGP1[[1]][[2]]
    $DGP1[[1]][[2]][[1]]
    [1] 2
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    attr(,"params")$y
    [1] 1
    
    
    $DGP1[[2]]
    $DGP1[[2]][[1]]
    $DGP1[[2]][[1]][[1]]
    [1] 3
    
    
    $DGP1[[2]][[2]]
    $DGP1[[2]][[2]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    attr(,"params")$y
    [1] 1
    
    
    $DGP1[[3]]
    $DGP1[[3]][[1]]
    $DGP1[[3]][[1]][[1]]
    [1] 2
    
    
    $DGP1[[3]][[2]]
    $DGP1[[3]][[2]][[1]]
    [1] 2
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    attr(,"params")$y
    [1] 2
    
    
    $DGP1[[4]]
    $DGP1[[4]][[1]]
    $DGP1[[4]][[1]][[1]]
    [1] 3
    
    
    $DGP1[[4]][[2]]
    $DGP1[[4]][[2]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    attr(,"params")$y
    [1] 2
    
    
    
    $DGP2
    $DGP2[[1]]
    $DGP2[[1]][[1]]
    $DGP2[[1]][[1]][[1]]
    [1] 3
    
    
    $DGP2[[1]][[2]]
    $DGP2[[1]][[2]][[1]]
    [1] 3
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 1
    
    
    $DGP2[[2]]
    $DGP2[[2]][[1]]
    $DGP2[[2]][[1]][[1]]
    [1] 4
    
    
    $DGP2[[2]][[2]]
    $DGP2[[2]][[2]][[1]]
    [1] 4
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 2
    
    
    $DGP2[[3]]
    $DGP2[[3]][[1]]
    $DGP2[[3]][[1]][[1]]
    [1] 5
    
    
    $DGP2[[3]][[2]]
    $DGP2[[3]][[2]][[1]]
    [1] 5
    
    
    attr(,"params")
    attr(,"params")$x
    [1] 3
    
    
    

