00010 ! Replace test\getdir
00020   library 'S:\Core\Library': fngetdir
00030   dim fl$(300)*99,dir$*80
00031   let dir$=env$('Q')&"\UBmstr"
00040   let fngetdir(dir$, mat fl$,' /s ','*.rtf')
00050 L50: if fl$(x+=1)<>'' then print fl$(x) : goto L50
00060   end 
