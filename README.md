# Prediction-Error-Estimation-in-Random-Forests

File Directory:
- big-func.R, big-func2.R, batch.sh, and batch2.sh are to run on the ITS server. They are the same thing just copied over to be able to run two different parameter combinations at the same time
- oob.setup.R: only contains a function called mtry.tune that takes in training data and outputs the model with the lowest OOB error
- data.generation.R: contains three functions
  - data.generation: takes in number of observations, number of variables, and class proportion and outputs a two-class dataset with these parameters where all variables have signal
  - data.generation.noisy: same as data.generation, but in the case of p=10 only the first 2 variables have signal and the rest are noise and in the case of p=100 the first 10 variables have signal
  - bates.data.generation.noisy: similar to data.generation.noisy, but the first 20% of variables have signal and the rest are noise
- fdo.R, fdcv.R, sdt.R, sdcv.R, lgcv.R all have a similar setup. Their input is a dummy variable just to be able to map the function many times. They all create a dataset and also a large holdout set that is later used for Err_XY. The rest is a little different across each file but it is pretty straight-forward
