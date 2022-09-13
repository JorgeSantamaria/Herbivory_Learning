# Herbivory_Learning
This is the first release of the code and data used to analyze data for the article: "Learning takes time: Biotic resistance by native herbivores increases through the invasion process."

# Assessment of <i>Sarpa salpa</i> herbivory on <i>Caulerpa cylindracea</i> through the invasive process

## Article title:
Learning takes time: Biotic resistance by native herbivores increases through the invasion process.

#### Journal:
Accepted in <i>Ecology Letters</i>

##### Authors:
Jorge Santamaría, Raül Golo, Jana Verdura, Fiona Tomas, Enric Ballesteros, Teresa Alcoverro, Rohan Arthur, Emma Cebrian

##### Script authors:
J. Santamaría (jsantamaria@ceab.csic.es)


## Abstract

As invasive species spread, the ability of local communities to resist invasion depends on the strength of biotic interactions. Evolutionarily unused to the invader, native predators or herbivores may be initially wary of consuming newcomers, allowing them to proliferate. However, these relationships may be highly dynamic, and novel consumer-resource interactions could form as familiarity grows. Here, we explore the development of effective biotic resistance towards a highly invasive alga using multiple space-for-time approaches. We show that the principal native Mediterranean herbivore learns to consume the invader within less than a decade. At recently invaded sites, the herbivore actively avoided the alga, shifting to distinct preference and high consumptions at older sites. This rapid strengthening of the interaction contributed to the eventual collapse of the alga after an initial dominance. Therefore, our results stress the importance of conserving key native populations to allow communities to develop effective resistance mechanisms against invaders.


## Methods

1) To assess if <i>Sarpa salpa</i>´s preference for <i>Caulerpa cylindracea</i> changed with exposure time and/or in relation to the abundance of the invader in the assemblage, we conducted paired-choice feeding experiments at different locations in Menorca and the Catalan coast, where it was possible to find locations with contrasting times since the invasion and with contrasting abundances of the invader. Paired-choice feeding experiments were performed in 8 different locations to represent 4 conditions of invasion time and abundance: Old-High, Old-Low, Recent-High, Recent-Low.
At each location, we conducted paired-choice feeding experiments to compare the relative palatability of <i>C. cylindracea</i> vs. two native macroalgae species: <i>Cystoseira compressa</i> and <i>Padina pavonica</i>. Thus, we used 2 treatments: i) Caulerpa – Cystoseira and ii) Caulerpa – Padina; and 3 controls, one per algal species. A total of 7 replicates for each treatment and 5 replicates for each control were deployed at each of the 8 locations. Replicate pairs were placed 1m apart from each other, while treatments were placed less than 20m apart to maintain constant environmental conditions between them. Samples were deployed in the morning and collected after 24h. Before and after deployment, every algal fragment was pat-dried of excess water and wet weighed to the nearest 0.01g. In all pairs, similar initial weights for each alga were offered to herbivores. Biomass consumption was estimated with the formula: (H_i x (C_f/C_i))-H_f,
where Hi and Hf were the initial and final wet weights of algae exposed to herbivory and Ci and Cf were initial and final mean wet weights of the controls. Consumption values were then standardized to percentage of consumed algae.

2) To assess if i) the percent of fish feeding on the invader and ii) the per capita consumption rates on the invasive alga (total amount consumed), change in relation to time since invasion and/or in relation to the abundance of the invader in the assemblage, fish fecal pellets were collected in the field (on SCUBA) from the same locations where the preference assessments were done. At each location, the day after completion of the preference experiment, we followed schools of <i>S. salpa</i> across their depth range and collected fecal pellets from the water column in individual zip bags while swimming below the fish. Between 30 and 50 pellets were collected per location. 
The presence (% of fish feeding in the invader) and abundance (per capita consumption rates) of <i>C. cylindracea</i> was determined by examining fecal pellets in a reticulated Petri dish under a stereomicroscope . Pellet content was spread uniformly on the dish and the relative abundance of <i>C. cylindracea</i> in each pellet was estimated as the mean percentage cover that it occupied in relation to the other content. 

3) To assess if <i>S. salpa</i>´s electivity towards <i>C. cylindracea</i> changed in relation to exposure time, the Ivlev´s Electivity Index (E) was calculated in two locations: Roses, at the 2nd, 3rd and 4th year after <i>C. cylindracea</i> invasion – first record in 2016; and Cabrera Archipelago, at the 4th, 5th and 17th year after the arrival of <i>C. cylindracea</i> – first record in 2003.
To determine E at each location for each time period, the following formula was used:E =  ((d_i-a_1))/((d_i+a_i)), where di = % of <i>C. cylindracea</i> in the fecal pellets of <i>S. salpa</i> (see the consumption assessment section) and ai = % of C. cylindracea available in the environment (see the previous section). The values of Ivlev´s Index (E) range from -1 (complete avoidance) to +1 (exclusive selection), with positive values indicating that the food item is selected and eaten more than it is encountered by chance in the environment.



## Usage Notes

### Herbivory_Learning.R
Rcode to: i) Assess the preference of <i>Sarpa salpa</i> towards invasive and native species through the invasion by means of paired Student t-tests and Wilcoxon tests; to ii) Analyze <i>Sarpa salpa</i> consumption of <i>Caulerpa cylindracea</i> through the invasive process by means of Generalized Linear Models (GLM) and to: iii) Assess the potential differences in the Electivity Indexes of <i>Sarpa salpa</i> towards <i>Caulerpa cylindracea</i> through the invasion by means of Kruskall-Wallis tests and Dunn tests. 


### Preference_Assessment.RData
Dataset with information on the consumption of an invasive alga species (<i>Caulerpa cylindracea</i>) and two native alga species (<i>Padina pavonica</i> and <i>Cystoseira compressa</i> )from the paired-choice feeding experiments performed at 8 different locations to compare the palatability between the invasive and the native species.It is needed to perform the assessment on the preference of <i>Sarpa salpa </i> through the invasion. 


### Algae_Consumption.RData
Dataset with information on the presence and abundance of <i>Caulerpa cylindracea</i> in the faecal pellets of <i>Sarpa salpa</i> collected at 9 different locations. It is needed to perform the statistical models to analyse the consumption of the invader and how this consumption has changed through the invasion.


### Ivlev_Comparison.RData
Dataset with information on the abundance of <i>Caulerpa cylindracea</i> in the faecal pellets of <i>Sarpa salpa</i> and in the environment at two different locations and at three different stages through the invasion.It is needed to calculate the Electivity Indexes of the herbivore towards the invaders and to assess the potential differences in the electivity through the invasive process.
