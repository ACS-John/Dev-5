! change 5
pr newpage
pr f '#0,1,33,C': 'problem exists in 4.31ib'
pr f '#0,2,33,C': 'problem exists in 4.31i'
pr f '#0,3,33,C': 'works fine in 4.31hg'
pr f '#0,5,33,C': 'wbversion$: '&wbversion$
amount$='52.13'
pr f '#0,8,33,C': 'try to type 123.45'
rinput fields '#0,9,33,10/#PIC(------.--),T[textboxes],300': amount$
