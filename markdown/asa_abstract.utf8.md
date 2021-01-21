---
title: "Black Lives Matter Protests and Voter Turnout"
thanks: The authors thank XX for their feedback and support. All errors are our responsibility. 
author:
- Cameron Kimble^[XX]
- Kevin Morris^[XX]
- Kasey Zapatka^[XX}
date: "January 21, 2021"
output:
  bookdown::pdf_document2:
    toc: no
    fig_caption: yes
    latex_engine: xelatex
    keep_tex: true
bibliography: "blm_turnout.bib"
link-citations: yes
fontsize: 12pt
header-includes:
    - \usepackage{rotating}
    - \usepackage{setspace}
abstract: "This is an abstract."
---
\pagenumbering{gobble}
\pagebreak

\pagenumbering{arabic}
\begin{center}
Black Lives Matter Protests and Voter Turnout
Cameron Kimble, Kevin Morris, and Kasey Zapatka
\end{center}

### Introduction {-}
TKTKTKTK
Political socialization, in recent years, is more and more defined as the gradual development of an individual’s political preferences, and more broadly, their perceptions of the political world – the norms and behaviors internalized within a given society [@Fillieule2013]. However, socialization research in the past has focused on why people do or do not participate in politics, as opposed to the socialization effects of participation in explicitly political organizations.
Given the resurgence of mass protest, we seek to understand the socializing effects of participation in social movements (or protest movements). Prior research [@Snow1986] indicates that initial frame alignment (“schemata of interpretation”) between an individual and a social movement organization is a precondition of participation, and that frame alignment is a continuing process that occurs between individuals and social movements, transforming and reinforcing the individual’s ideological orientations.
Research on American activists in the 1960s has addressed the effects of participation on a social movement on political behaviors and views. Indeed, one study found that participating in the Freedom Summer project caused volunteers to become “radicalized,” and were thus significantly more likely to participate in social movements in subsequent years [@McAdam1989]. In a similar study, researchers conclude that involvement with social movements through the 1960s and 1970s caused participants to become significantly more liberal in political orientation, as evidenced by subsequent political engagement – voting for Jimmy Carter, and participating in subsequent demonstrations  [@Sherkat1997].
 Harnessing discontent, and channeling it into political/social movement
-Rustin
 Raising awareness/developing critical conscious through Social movement participation 

### Literature on Protests and Mobilization {-}
Cameron to fill

### The Mobilizing Potential of Injustice Narratives {-}

Although protests can be a site of key political socialization, the Black Lives Matter movement centers on a social issue that has historically been *demobilizing.* Much research over the past decade has demonstrated that contact with the legal system causes individuals to withdraw from political socialization [see, for instance, @Lerman2014]. These demobilizing effects may extend even to individuals who have *not* been formally convicted, but rather are in close contact with those who have been [@Morris2020; but see @White2019a]. The relationship, however, is complicated: @Walker2014 shows that individuals in “proximal contact” --- whose family members have been convicted --- become more likely to participate in non-electoral political activities.

@Walker2020, however, provides key insight into how the generally demobilizing effects of contact with the legal system can be transformed into a *mobilizing* force. Walker argues that, when individuals locate their experience with the legal system in narratives of (racial) injustice, the contact can cause them to take action. The Black Lives Matter movement aimed to do just this. BLM activists called attention to the fact that American police forces have a long history of racially discriminatory practices. The widespread protests may have caused individuals in close contact with police and the legal system to understand their contact with the police and legal systems *not* as personal failings but as a key producer of racial inequity. The recent work from Hannah Walker indicates that this may have been a mobilizing event.

### Data and Methods {-}

To understand the relationship between protests and turnout, we leverage two primary datasets. First, we employ data collected by the US Crisis Monitor^[See https://acleddata.com/download/22846/.] which records the location of every Black Lives Matter protest during 2020. These events are geocoded, which allow us to identify the precise location of each protest.

Our second primary data source is the national, geocoded voter file made available by L2 Political. The voter file includes a host of information about every registered voter in the country, including their gender, age, partisan affiliation, and race (L2 models the characteristics that are not reported in each state’s public voter file). The registered voter file also includes historical voter turnout for both the 2020 and historical elections. At this point, not all states have yet entered their 2020 voter turnout into the registered voter file.

We intend to measure whether how far a voter lived from a Black Lives Matter protest in the summer of 2020 was correlated with their turnout in November. Using these geocoded records, we measure the distance between each voter and the closest protest, and test whether this distance is significantly related to turnout after controlling for other relevant characteristics, *including* past turnout.

To be sure, this cannot prove the causal link between exposure to protest and increased turnout. Voters were not “randomly” exposed to protests; the same factors that primed an area to stage a protest may have also primed them to turn out at higher rates in the 2020 election. To identify the causal effect of exposure to protest on voter turnout we therefore leverage variation in rainfall (relative to historical rainfall) in the week following the murder of George Floyd in an instrumental variable framework. We expect that areas with higher rainfall likely had fewer protests, *regardless* of whether that area was likely to turn out at high rates in November. For a fuller discussion of how rainfall can be used to instrument exposure to political protests, see @Madestam2013.

### Preliminary Results {-}

As mentioned above, individual-level turnout records are not yet widely available, but will be published in the first half of 2021. The records are available, however, for the states of Georgia and North Carolina which we use as pilot studies.

Figure \ref{fig:map} displays the location of all protests staged in June, 2020, in Georgia and North Carolina. We also show the distribution of voters around the state.

\begin{figure}[H]
\includegraphics[width=0.5\linewidth]{asa_abstract_files/figure-latex/figures-side-1} \includegraphics[width=0.5\linewidth]{asa_abstract_files/figure-latex/figures-side-2} \caption{\label{fig:map}Sites of June, 2020, Protests}(\#fig:figures-side)
\end{figure}

In Table \ref{tab:reg} we present the results of an ordinary least-squares regression, where each observation is a registered voter. The dependent variable *Voted in 2020* measures whether a voter participated in the 2020 general election, and the primary dependent variable *Distance* measures how far the voter lived from the nearest June protest in their state. We also control for other individual- and neighborhood-level characteristics, including each voter’s own participation history. Robust standard errors are clustered by county.^[These are currently run on random 10 percent samples, stratified by county, due to computing restraints. The models will be run on the full universe of voters in the full paper.]

\begin{singlespace}
\input{"../temp/big_reg_formatted.tex"}
\end{singlespace}

### What Comes Next {-}

Our results from Georgia indicate that the relationship between distance to protests and turnout is more complicated than we initially expected. This is perhaps true for both methodological reasons and for theoretical ones. Methodologically, we have not yet implemented the IV model which will allow us to more precisely estimate the causal effect of protest exposure. Moreover, Figure \ref{fig:map} demonstrates that there is clear spatial patterning in the siting of protests; we need to implement spatial regression models to fully account for this. Theoretically, it may be that exposure to protest has different effects on different voters, and that a single estimate of exposure is muddying heterogeneous effects; our full paper will explore this potentiality.

\newpage
### References {-}
