
generateTrainTest = function(df, train_test_split_bet_0_and_1){
    set.seed(3)
    sample <- sample.int(n = nrow(df), size = floor(train_test_split_bet_0_and_1*nrow(df)), replace = F)
    train <- df[sample, ]
    test  <- df[-sample, ]
    assign('train', train, envir=parent.frame())
    assign('test', test, envir=parent.frame())
}

FONT = "Times New Roman"
windowsFonts(FONT = windowsFont(FONT))
