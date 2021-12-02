# Midterm-Coding-Project

How to run program: 
- download the zip file 
- open MidtermProject.Rproj in r studio 
- run vg_analysis.Rmd 
* Please note that it may take a few minutes to run all the machien learning models towards the end of the code because of model complexity (specifically the random forest model may take the longest)


Dataset: https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings


References:
https://gexijin.github.io/learnR/the-game-sales-dataset.html
https://stackoverflow.com/questions/14622421/how-to-change-legend-title-in-ggplot
https://intellipaat.com/community/16343/how-to-put-labels-over-geombar-for-each-bar-in-r-with-ggplot2
https://www.color-hex.com/
https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot
http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html
https://www.kaggle.com/yonatanrabinovich/video-games-sales-regression-techniques
https://www.kaggle.com/tnyont/sales-of-video-games-analysis-visualization
https://www.kaggle.com/rohitbokade94/analysis-of-videogame-sales



Things I plan on adding/ working on:
- going to add a rmd file as well
- adding more descriptive comments after each chunk of code 
- adding a few more graphs
- improving graphs further (adding theme_minimal, seeing if I can better convey the data in a different type of graph, titles, coloring the bars etc)
- hyper parameter tuning for all the models 
- seeing which models I should keep/get rid of 
- adding different measurements for models (maybe accuracy, mse, mae, etc)


Things I worked on since first submission:
- added a detailed RMD file 
- added a PDF file as well of the RMD file 
- added several testthats
- added overall objectives before the code (in the RMD file)
- added more in depth comments throughout different portions of the code
- added more information to graphs, improved some data visualization and added titles
- did not get rid of any of the model because I felt that it was important to see the different performances
- also did not end up adding different measurements besides RMSE because it shows us the standard deviation of the predicted errors which is exactly what I wanted to analyze and showcase
- did quite a bit of hyperparameter tuning, but the original models made during the first submission ended up working the best
- did change the train/test split to 80-20 instead of 90-10 which managed to improve the RMSE slightly 
