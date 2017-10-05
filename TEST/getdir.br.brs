00010 ! Replace test\getdir
00020   library 'S:\Core\Library': fngetdir
00022   library 'S:\Core\Library': fngetdir2
00030   dim fl$(300)*99,dir$*80
00031   dir$=env$('Q')&"\UBmstr"
00040   fngetdir2(dir$, mat fl$,' /s ','*.rtf')
48000   for x=1 to udim(mat fl$)
48020     if fl$(x+=1)<>'' then '(return$('&str$(x)&')="'&fl$(x)&'"'
48040   nex x
