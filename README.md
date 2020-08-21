This folder contains all information and code to replicate findings and figures presented in 
[Neunhoeffer, Marcel/ Gschwend, Thomas/ Munzert, Simon/ Stoetzer, Lukas F. (2020): 
'An Approach to Predict the District Vote Shares in German Federal Elections' 
in: German Political Science Quarterly 61, 111–130.](https://link.springer.com/content/pdf/10.1007/s11615-019-00216-3.pdf)

- figures: 		.tiff files of figures presented in paper
- manuscript: 		final paper as published and online appendix
- processed-data: 	final results of analysis
- raw-data: 		raw data sets used to run the models
- scripts: 		R-code allowing to replicate the analysis

In case of questions please contact the corresponding author Marcel Neunhoeffer: [mneunhoe@mail.uni-mannheim.de](mailto:mneunhoe@mail.uni-mannheim.de)

Due to privacy concerns the candidate data needed to reproduce the analyses is not included in this repository. This data can be obtained for research purposes from the [Bundeswahlleiter](https://www.bundeswahlleiter.de/info/kontakt.html). The required structure of the data sets can be found in the `raw-data/btw<xxxx>bewerb_clean.csv` files, where <xxxx> stands for the election year. Pre-processing code for the files is included in `scripts/02-data-preprocessing.R`.
