

# Per Analysis Plan in preregistration: "To test whether note taking 
# condition influences word count, we will compute one independent 
# samples t-test to compare the two note taking conditions (longhand, 
# laptop) across video conditions."

# Welch's t-tests to examine effects of note taking condition 
# on word count
ttest_laplong_wc <- t.test(wordcount~laplong, 
                           mydata)

# same but without influential observations
ttest_laplong_wc_noout <- t.test(wordcount~laplong, 
                                 subset(mydata, filter_wordcount == 1))

# Per Analysis Plan in preregistration: "If we are able to obtain the 
# relevant software, we will test whether note taking condition influences
# level of verbatim overlap between participant notes and the original
# lectures (one-gram, two-gram, three-gram). To do so, we will compute 
# three independent samples t-tests to compare the two note taking 
# conditions (longhand, laptop) across video conditions."

# Here we deviate from the preregistered plan and focus only on the
# the three-gram condition for the sake of simplicity. The one-gram and 
# two-gram measures produced the same finding in the original report.

# Welch's t-tests to examine effects of note taking condition 
# on verbatim overlap
ttest_laplong_vo <- t.test(threegramspercent~laplong, mydata)

# same but without influential observations
ttest_laplong_vo_noout <- t.test(threegramspercent~laplong, 
                                 subset(mydata, filter_threegramspercent == 1))

