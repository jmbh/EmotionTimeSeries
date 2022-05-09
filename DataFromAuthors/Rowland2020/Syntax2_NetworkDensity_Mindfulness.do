import delimited "data_net_density.csv", clear


///Table 1
///Intercorrelations
pwcorr t_oad t_nad t_pad smaas, sig


///Table 2
///Hierarchical mutiple regression: temporal overall affect network density
nestreg: reg t_oad (gc_smaas group) (mind) ///
(smaas_mind group_mind), beta 


///Table 3
///Hierarchical mutiple regression: temporal positive affect network density
nestreg: reg t_pad (gc_smaas group) (mind) ///
(smaas_mind group_mind), beta 

///Hierarchical mutiple regression: temporal negative affect network density
nestreg: reg t_nad (gc_smaas group) (mind) ///
(smaas_mind group_mind), beta 


///Figure 2
regress t_oad c.gc_smaas i.group i.mind c.gc_smaas#i.mind i.group#i.mind, beta
margins mind, at(gc_smaas=(-.879 .879))
marginsplot


///Supplementary material
///Table S1
///Intercorrelations
pwcorr t_oad t_nad t_pad c_oad  c_pad c_nad c_pa_nad, sig 

///Table S2
///Hierarchical mutiple regression: contemporaneous overall affect network density
nestreg: reg c_oad (gc_smaas group) (group mind) ///
(smaas_mind group_mind), beta

///Table S3
///Hierarchical mutiple regression: contemporaneous positive affect network density
nestreg: reg c_pad (gc_smaas group) (mind) ///
(smaas_mind group_mind), beta

///Hierarchical mutiple regression: contemporaneous negative affect network density
nestreg: reg c_nad (gc_smaas group) (mind) ///
(smaas_mind group_mind), beta


///Table S4
///Hierarchical mutiple regressions: contemporaneous positive-negative affect network density
nestreg: reg c_pa_nad (gc_smaas group) (mind) ///
(smaas_mind group_mind), beta


///Table S5
///Alternative meditation variable
nestreg: reg t_oad (group gc_smaas) (both_mind) ///
(group_both_mind smaas_both_mind), beta

///Table S6
///Alternative meditation variable
nestreg: reg t_pad (group gc_smaas) (both_mind) ///
(group_both_mind smaas_both_mind ), beta

nestreg: reg t_nad (group gc_smaas) (both_mind) ///
(group_both_mind smaas_both_mind ), beta 


///Table S7
///Hierarchical mutiple regressions: single-affect density
nestreg: reg t_happy (group gc_smaas mind) (smaas_mind group_mind), beta
nestreg: reg t_excited (group gc_smaas mind) (smaas_mind group_mind), beta
nestreg: reg t_relaxed (group gc_smaas mind) (smaas_mind group_mind), beta
nestreg: reg t_satisfied (group gc_smaas mind) (smaas_mind group_mind), beta
nestreg: reg t_angry (group gc_smaas mind) (smaas_mind group_mind), beta
nestreg: reg t_anxious (group gc_smaas mind) (smaas_mind group_mind), beta
nestreg: reg t_depressed (group gc_smaas mind) (smaas_mind group_mind), beta
nestreg: reg t_sad (group gc_smaas mind) (smaas_mind group_mind), beta




