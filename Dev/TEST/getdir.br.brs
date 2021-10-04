! Replace test\getdir
	autoLibrary
	dim fl$(300)*99,dir$*80
	dir$="[Q]\UBmstr"
	fngetdir2(dir$, mat fl$,' /s ','*.rtf')
	for x=1 to udim(mat fl$)
		if fl$(x+=1)<>'' then '(return$('&str$(x)&')="'&fl$(x)&'"'
	nex x
