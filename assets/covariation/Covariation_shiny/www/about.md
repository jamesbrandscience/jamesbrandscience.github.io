### Systematic co-variation of monophthongs across speakers of New Zealand English

This Shiny app allows users to interactively explore the results of the PCA analysis 
reported in the paper.

It provides visualisations of the data that will help explain and assist with the 
interpretation of the 3 main principal components (click on the `PCA explorer` tab 
in the main navigation bar at the top).

Each principal component can be explained by 3 plots:

* **Speaker loadings (left plot)**

	This plot presents each individual speaker in the dataset (represented by the **F** 
	or <span style="color: #75FC4C">**M**</span> points, for female and male speakers 
	respectively), with their year of birth plotted on the x axis and the speaker loading 
	for the component on the y axis.

	This plot is similar to ---FIG REF--- of the paper. By clicking on a speaker you can 
	access their demographic information.
	
* **Vowel space (middle plot)**

	This plot is reactive and responds to the speaker you click on in the loadings plot. 
	When you click on a speaker, this will generate their vowel space, with raw F1/F2 
	values on the y/x axis respectively. The vocalic variables that are considered 
	important to the principal component are presented in colour, all other variables are 
	in gray.
	
* **Vocalic variables schematic (right plot)**

	This plots gives an animation of the key vocalic variables that contribute to the 
	principal component (i.e. have a loading > |2|). The animation functions to assist 
	the user with their interpretation of the principal component, by showing how speaker 
	vowel spaces may differ based on the speaker loadings.
	
	If a vocalic variable is moving vertically, this indicates change in the vowel's F1 
	values. If it is moving horizontally, this indicates change in F2 values. If it is 
	moving diagonally, then this would indicate change in both F1 and F2 values.