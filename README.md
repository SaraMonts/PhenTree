
</br>

<img src="www/app_logo.png"/>

</br>
</br>

You can find PhenTree here: https://saramonts.shinyapps.io/PhenTree/

</br>


### What is PhenTree?

PhenTree is a completely free web application to generate pedigrees from a genetic point of view. It's purpose is to help clinical geneticist and researchers to represent the family history of patients to better diagnose a disease, calculate risks for individuals, represent case studies in scientific publications, and many other uses. 

PhenTree follows the Standardized Human Pedigree Nomenclature defined by the National Society of Genetic Counselors on 1995 [1].

</br>


### How was PhenTree created?

PhenTree is based in R programming language and uses mainly two R packages:

* **kinship2**: created by Jason Sinnwell and Terry Therneau, offers functions to generate basic pedigree plots [2]. Some extra symbols and customization options were added to this R package. 
* **shiny**: offers functions to facilitate the creation of interactive web apps using R [3]. 

PhenTree also makes use of other R packages like **shinyjs** [4], **shinyalert** [5] and **colourpicker** [6] by Dean Attali, as well as other common R packages (**RMySQL** [7], **pool** [8] and **DT** [9]). Moreover, all this app information has been written using **markdown** [10]. 

</br>


### What are the steps to create a pedigree?

The following steps are only an example of the process to generate a pedigree. However, steps 4 to 7 can be performed in any order you like.

</br>


#### STEP 1: Register

Enter a username and a password of your choosing and click the *Sign in* button. Remember them well!! For now we do not offer any option to recover them.

<kbd>
<img src="www/register.png" width="230"/>
</kbd>
</br>
</br>


#### STEP 2: Log in

Once you created a user account, you can log in with your username and your password.

<kbd>
<img src="www/login.png" width="230"/>
</kbd>
</br>
</br>


#### STEP 3: Create a pedigree

Create a new pedigree by clicking at the *Create a new pedigree* button and entering a name for the pedigree. Once you have many pedigrees created, you can select the one you want to work with or delete the ones that you no longer need. 

<kbd>
<img src="www/newpedigree.png" width="250"/>
</kbd>
</br>
</br>


#### STEP 4: Define the phenotypes

In the *Define the phenotypes* tab of the left panel (we'll call it **inputs panel**) you can select the phenotypes you would like to represent in the pedigree for this particular family. The names of the phenotypes and their IDs are the standards defined by the [Human Phenotype Ontology](https://hpo.jax.org/app/) [11]. You can select up to four. 

<kbd>
<img src="www/definephens.png" width="250"/>
</kbd>
</br>
</br>


#### STEP 5: Add family members

In the *Add family members* tab of the inputs panel there is a form where you can add the information of every person of the family one at a time. Once you press *Submit*, a new row appears in the *Summary table* of the right panel (we'll call it **outputs panel**) with the information you introduced for that person. You can add as many family members as you like. 

<kbd>
<img src="www/addmembers.png" width="250"/>
</kbd>
</br>
</br>

You can edit, delete or change the order of the rows in the summary table at any time using the buttons above it. 

<kbd>
<img src="www/summarytable.png" width="500"/>
</kbd>
</br>
</br>

The recommendation is to introduce the members from oldest to youngest, although it is not obligatory. If you introduce a child before introducing their parents, you'll have to later edit the child's information to indicate who their parents are, which takes more time. 

The pedigree is automatically generated and updated with the information contained in the summary table and is shown in the *Pedigree* tab of the outputs panel. Under the pedigree you can also see a legend of the symbols and options to download the image. 

<kbd>
<img src="www/pedigree.png" width="500"/>
</kbd>
</br>
</br>


#### STEP 6: Customize your pedigree

In the *Customize pedigree* tab of the inputs panel there are many options to personalize the pedigree plot as you like. You can choose which information to add in the plot, the sizes, the colors and if you would like to view the legend or not. 

<kbd>
<img src="www/customped.png" width="250"/>
</kbd>
</br>
</br>


#### STEP 7: Get information about your phenotypes of interest

In the *Genes and diseases* tab of the outputs panel you can find, for every one of the phenotypes you selected, a table that contains the genes and the respective diseases that have been associated to this phenotype according to the Human Phenotype Ontology [11]. In the *Disease ID* column there is a link to the [Orphanet](https://www.orpha.net/consor/cgi-bin/index.php) [12] or the [Online Mendelian Inheritance in Man (OMIM)](https://www.omim.org/) [13] websites where you can find more information about every disease and the genes associated to it. You can also download these tables as a CSV file or Excel file. 

<kbd>
<img src="www/tablegenes.png" width="500"/>
</kbd>
</br>
</br>
</br>




### References

[1] Bennett RL, Steinhaus KA, Uhrich SB, O’Sullivan CK, Resta RG, Lochner-Doyle D, et al. Recommendations for standardized human pedigree nomenclature. Am J Hum Genet. 1995;56(3):745-52

[2] Sinnwell J, Therneau T. kinship2: Pedigree Functions. R package version 1.8.5. 2020. Available from: https://cran.r-project.org/package=kinship2

[3] Chang W, Cheng J, Allaire J, Xie Y, McPherson J. shiny: Web Application Framework for R. R package version 1.5.0. 2020. Available from: https://cran.r-project.org/package=shiny

[4] Attali D. shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 2.0.0. 2020. Available from: https://cran.r-project.org/package=shinyjs

[5] Attali D, Edwards T. shinyalert: Easily Create Pretty Popup Messages (Modals) in Shiny. R package version 2.0.0. 2020. Available from: https://cran.r-project.org/package=shinyalert

[6] Attali D. colourpicker: A Colour Picker Tool for Shiny and for Selecting Colours in Plots. R package version 1.1.0. 2020. Available from: https://cran.r-project.org/package=colourpicker

[7] Ooms J, James D, DebRoy S, Wickham H, Horner J. RMySQL: Database Interface and MySQL Driver for R. R package version 0.10.20. 2020. Available from: https://cran.r-project.org/package=RMySQL

[8] Cheng J, Borges B. pool: Object Pooling. R package version 0.1.4.3. 2019. Available from: https://cran.r-project.org/package=pool

[9] Xie Y, Cheng J, Tan X. DT: A Wrapper of the JavaScript Library DataTables. R package version 0.15. 2020. Available from: https://cran.r-project.org/package=DT

[10] Allaire J, Horner J, Xie Y, Marti V, Porte N. markdown: Render Markdown with the C Library Sundown. R package version 1.1. 2019. Available from: https://cran.r-project.org/package=markdown

[11] Human Phenotype Ontology Consortium. Human Phenotype Ontology [Internet]. 2020 [cited 2020 november 20]. Available from: https://hpo.jax.org/app/

[12] Orphanet. Orphanet [Internet]. 2020 [cited 2020 november 20]. Available from: https://www.orpha.net/consor/cgi-bin/index.php

[13] Johns Hopkins University. OMIM - Online Mendelian Inheritance in Man [Internet]. 2020 [cited 2020 november 20]. Available from: https://www.omim.org/








