library program$: fnDedNames
dim xa$(20)*20,xb$(20)*8,xc(20)
fnDedNames(mat xa$,mat xb$, mat xc) ! ,mat xd,mat xe,mat xf,mat xg,mat xh,mat xi$)
pr 'fullName','abbrName','dedcode'
for x=1 to 20
	pr xa$(x),xb$(x),xc(x)
nex x
pause
end
def library fnDedNames(mat fullName$; mat abrevName$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat dedGl$,doWrite)
	if doWrite then ! r:
		open #hDedNames=fnH: 'Name=[Q]\PRmstr\DedNames.h[cno],RecL=920,use',i,outi,r
		rewrite #hDedNames,using fDedNames,rec=1: mat fullName$, mat abrevName$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat dedGl$
		close #hDedNames:
		doWrite=0
	end if ! /r

	if ~setupDedNames then ! r:
		setupDedNames=1
		autoLibrary
		dim cache_fullName$(20)*20
		dim cache_abrevName$(20)*8
		dim cache_dedcode(20)
		dim cache_calcode(20)
		dim cache_dedFed(20)
		dim cache_dedFica(20)
		dim cache_dedSt(20)
		dim cache_deduc(20)
		dim cachededGl$(20)

		mat cache_fullName$  	=('')
		mat cache_abrevName$ 	=('')
		mat cache_dedcode    	=(0)
		mat cache_calcode    	=(0)
		mat cache_dedFed     	=(0)
		mat cache_dedFica    	=(0)
		mat cache_dedSt      	=(0)
		mat cache_deduc      	=(0)
		mat cache_deduc      	=(0)
		mat cachededGl$      	=('')

		if exists('[Q]\PRmstr\DedNames.h[cno]') then
			open #hDedNames=fnH: 'Name=[Q]\PRmstr\DedNames.h[cno]',i,i,r
			read #hDedNames,using fDedNames,rec=1: mat cache_fullName$,mat cache_abrevName$,mat cache_dedcode,mat cache_calcode,mat cache_dedFed,mat cache_dedFica,mat cache_dedSt,mat cache_deduc,mat cachededGl$
			fDedNames: form pos 1,20*C 20,20*C 8,120*N 1,20*C 12
		else
			open #hDedNames=fnH: 'Name=[Q]\PRmstr\DedNames.h[cno],RecL=920,use',i,outi,r
			write #hDedNames,using fDedNames: mat cache_fullName$,mat cache_abrevName$,mat cache_dedcode,mat cache_calcode,mat cache_dedFed,mat cache_dedFica,mat cache_dedSt,mat cache_deduc,mat cachededGl$
		end if
		close #hDedNames:
	end if ! /r

	mat fullName$(udim(mat cache_fullName$))
	for fullNameItem=1 to udim(mat fullName$)
		fullName$(fullNameItem)=cache_fullName$(fullNameItem) soflow ignore
	nex fullNameItem

	! on error goto DnFINIS !  assumes that the erroring one and all further paramters were not passed
	if fnArrayWasPassedC(mat abrevName$) then
		mat abrevName$(udim(mat cache_abrevName$))
		for abrevnameItem=1 to udim(mat abrevName$)
			abrevName$(abrevnameItem)=cache_abrevName$(abrevnameItem) soflow ignore
		nex abrevnameItem
	! else
	! 	pr 'not passed abrevName$'
	end if
	if fnArrayWasPassedN(mat dedcode	) then mat dedcode(udim(mat cache_dedcode))	: mat dedcode=cache_dedcode
	if fnArrayWasPassedN(mat calcode	) then mat calcode(udim(mat cache_calcode))	: mat calcode=cache_calcode
	if fnArrayWasPassedN(mat dedfed 	) then mat dedfed(udim(mat cache_dedFed))  	: mat dedfed=cache_dedFed
	if fnArrayWasPassedN(mat dedfica	) then mat dedfica(udim(mat cache_dedFica))	: mat dedfica=cache_dedFica
	if fnArrayWasPassedN(mat dedst  	) then mat dedst(udim(mat cache_dedSt))     	: mat dedst=cache_dedSt
	if fnArrayWasPassedN(mat deduc  	) then mat deduc(udim(mat cache_deduc))     	: mat deduc=cache_deduc
	if fnArrayWasPassedC(mat dedGl$ 	) then mat dedGl$(udim(mat cachededGl$))    	: mat dedGl$=cachededGl$
	! DnFINIS: !
fnend
