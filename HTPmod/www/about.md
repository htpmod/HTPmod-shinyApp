## HTPmod 
> ### A Shiny Application for Modeling and Visualization of Large-Scale Biological Data  
>> #### by Dijun Chen<sup>1,2</sup>, Liangyu Fu<sup>1</sup>, Dahui Hu<sup>3</sup>, Christian Klukas<sup>2</sup>, Ming Chen<sup>3</sup> and Kerstin Kaufmann<sup>1</sup>
>> ##### <sup>1</sup> Department for Plant Cell and Molecular Biology, Institute for Biology, Humboldt-Universität zu Berlin, 10115 Berlin, Germany. 
>> ##### <sup>2</sup> Leibniz Institute of Plant Genetics and Crop Plant Research (IPK), Corrensstrasse 3, 06466 Gatersleben, Germany. 
>> ##### <sup>3</sup> Department of Bioinformatics, College of Life Sciences, Zhejiang University, Hangzhou 310058, China. 
>>> HTPmod is an interactive web tool for modeling and visualization of large-scale datasets from high-throughput plant phenotyping (HTP) or high-throughput sequencing (HTS) experiments. It consists of three function modules.

> > > * **`growMod`** was developed for plant growth modeling based on time-series data, e.g., from HTP experiments<sup>[1]</sup>. 

> > > * **`predMod`** was implemented with several prediction models that can be used to understand which among the independent measurements are related to the desirable target trait, and to dissect the forms of these relationships. For example, such prediction models have been widely used to model the contribution of chromatin features to the change of gene expression<sup>[2-7]</sup>, and to predict plant biomass from image-derived features<sup>[8-12]</sup>. 

> > > * **`htpdVis`** was developed to explore and visualize high-dimensional data using machine learning approaches such as principal component analysis (PCA), t-distributed stochastic neighbor embedding (t-SNE), self-organizing map (SOM), multidimensional scaling (MDS), K-means clustering (K-MC) or hierarchical cluster analysis (HCA) with heatmap. 

> > > To get started, select `growMod`, `predMod` or `htpdVis` and read the instructions .


<div id="carousel" class="carousel slide" data-ride="carousel"> 
	<ol class="carousel-indicators">
		<li data-target="#carousel" data-slide-to="0" class="active"></li>
		<li data-target="#carousel" data-slide-to="1"></li>
		<li data-target="#carousel" data-slide-to="2"></li>
	</ol>
	<div class="carousel-inner" role="listbox">
		<div class="item active"> <img src="module1.png" class="center-block" alt="growMod" width='1024'>
			<div class="carousel-caption label label-default">
			    <h3>Plant Growth Modeling</h3>
                <p>Modeling of plant growth based on time-series data.</p>
			</div>
		</div>
		<div class="item"> <img src="module2.png" class="center-block" alt="predMod" width='1024'>
			<div class="carousel-caption label label-default">
			    <h3>Prediction Models</h3>
                <p>Predicting important traits based on high-dimensional data.</p>
			</div>
		</div>
		<div class="item"> <img src="module3.png" class="center-block" alt="htpdVis" width='1024'>
		    <div class="carousel-caption label label-default">
			    <h3>HT Data Visualization</h3>
                <p>Visualization of high-throughput data.</p>
			</div>
		</div>
	</div>
	<a class="left carousel-control" href="#carousel" role="button" data-slide="prev"> <span class="glyphicon glyphicon-chevron-left" aria-hidden="true"></span> <span class="sr-only">Previous</span> </a>
	<a class="right carousel-control" href="#carousel" role="button" data-slide="next"> <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span> <span class="sr-only">Next</span> </a>
</div>

### Source code 
> Source code is available <a href="https://github.com/htpmod/" target="_blank">on GitHub</a>

### Contact us 
> chendijun2012@gmail.com

### How to cite 
> HTPmod: a Shiny Application for Modeling and Visualization of Large-Scale Biological Data 

[comment]: ![](module3_800.png)

### References
> <sup>[1]</sup>Chen D, Neumann K, Friedel S, Kilian B, Chen M, Altmann T, Klukas C. Dissecting the phenotypic components of crop plant growth and drought responses based on high-throughput image analysis. The Plant Cell. 2014 Dec 1;26(12):4636-55. 

> <sup>[2]</sup>Cheng C, Alexander R, Min R, Leng J, Yip KY, Rozowsky J, Yan KK, Dong X, Djebali S, Ruan Y, Davis CA. Understanding transcriptional regulation by integrative analysis of transcription factor binding data. Genome research. 2012 Sep 1;22(9):1658-67. 

> <sup>[3]</sup>Cheng C, Gerstein M. Modeling the relative relationship of transcription factor binding and histone modifications to gene expression levels in mouse embryonic stem cells. Nucleic acids research. 2011 Sep 16;40(2):553-68.

> <sup>[4]</sup>Cheng C, Yan KK, Yip KY, Rozowsky J, Alexander R, Shou C, Gerstein M. A statistical framework for modeling gene expression using chromatin features and application to modENCODE datasets. Genome biology. 2011 Feb 16;12(2):R15.

> <sup>[5]</sup>Dong X, Greven MC, Kundaje A, Djebali S, Brown JB, Cheng C, Gingeras TR, Gerstein M, Guigó R, Birney E, Weng Z. Modeling gene expression using chromatin features in various cellular contexts. Genome biology. 2012 Sep 5;13(9):R53.

> <sup>[6]</sup>Karlić R, Chung HR, Lasserre J, Vlahoviček K, Vingron M. Histone modification levels are predictive for gene expression. Proceedings of the National Academy of Sciences. 2010 Feb 16;107(7):2926-31.

> <sup>[7]</sup>Song L, Huang SS, Wise A, Castanon R, Nery JR, Chen H, Watanabe M, Thomas J, Bar-Joseph Z, Ecker JR. A transcription factor hierarchy defines an environmental stress response network. Science. 2016 Nov 4;354(6312):aag1550.

> <sup>[8]</sup>Busemeyer L, Ruckelshausen A, Möller K, Melchinger AE, Alheit KV, Maurer HP, Hahn V, Weissmann EA, Reif JC, Würschum T. Precision phenotyping of biomass accumulation in triticale reveals temporal genetic patterns of regulation. Scientific reports. 2013 Aug 14;3:2442.

> <sup>[9]</sup>Chen D, Shi R, Pape JM, Klukas C. Predicting plant biomass accumulation from image-derived parameters. bioRxiv. 2016 Jan 1:046656. 

> <sup>[10]</sup>Fahlgren N, Feldman M, Gehan MA, Wilson MS, Shyu C, Bryant DW, Hill ST, McEntee CJ, Warnasooriya SN, Kumar I, Ficor T. A versatile phenotyping system and analytics platform reveals diverse temporal responses to water availability in Setaria. Molecular plant. 2015 Oct 5;8(10):1520-35.

> <sup>[11]</sup>Golzarian MR, Frick RA, Rajendran K, Berger B, Roy S, Tester M, Lun DS. Accurate inference of shoot biomass from high-throughput images of cereal plants. Plant Methods. 2011 Feb 1;7(1):2. 

> <sup>[12]</sup>Yang W, Guo Z, Huang C, Duan L, Chen G, Jiang N, Fang W, Feng H, Xie W, Lian X, Wang G. Combining high-throughput phenotyping and genome-wide association studies to reveal natural genetic variation in rice. Nature communications. 2014 Oct 8;5. 


<br/>
