00001   print newpage
00002   print fields '#0,1,33,C': 'problem exists in 4.31ib'
00003   print fields '#0,2,33,C': 'problem exists in 4.31i'
00004   print fields '#0,3,33,C': 'works fine in 4.31hg'
00005   print fields '#0,5,33,C': 'wbversion$: '&wbversion$
00010   let amount$='52.13'
00020   print fields '#0,8,33,C': 'try to type 123.45'
00030   rinput fields '#0,9,33,10/#PIC(------.--),T[textboxes],300': amount$
