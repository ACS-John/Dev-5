library program$: fnDedNames
dim xa$(20)*20,xb$(20)*8,xc(20)
fnDedNames(mat xa$,mat xb$, mat xc) ! ,mat xd,mat xe,mat xf,mat xg,mat xh,mat xi$)
pr 'fullname','abbrName','dedcode'
for x=1 to 20
	pr xa$(x),xb$(x),xc(x)
nex x
pause
end
def library fnDedNames(mat fullname$; mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$,doWrite)
	RESTART: !
	if ~setupDedNames then
		setupDedNames=1
		library 'S:\Core\Library': fnH
		library 'S:\Core\Library': fnArrayWasPassedC
		library 'S:\Core\Library': fnArrayWasPassedN
		dim cache_fullname$(20)*20
		dim cache_abrevname$(20)*8
		dim cache_dedcode(20)
		dim cache_calcode(20)
		dim cache_dedfed(20)
		dim cache_dedfica(20)
		dim cache_dedst(20)
		dim cache_deduc(20)
		dim cache_gl$(20)

		mat cache_fullname$  =('')
		mat cache_abrevname$ =('')
		mat cache_dedcode    =(0)
		mat cache_calcode    =(0)
		mat cache_dedfed     =(0)
		mat cache_dedfica    =(0)
		mat cache_dedst      =(0)
		mat cache_deduc      =(0)
		mat cache_deduc      =(0)
		mat cache_gl$        =('')

		if exists("[Q]\PRmstr\DedNames.h[cno]") then 
			open #hdednames=fnH: "Name=[Q]\PRmstr\DedNames.h[cno]",internal,input,relative 
			read #hdednames,using fDedNames,rec=1: mat cache_fullname$,mat cache_abrevname$,mat cache_dedcode,mat cache_calcode,mat cache_dedfed,mat cache_dedfica,mat cache_dedst,mat cache_deduc,mat cache_gl$
			fDedNames: Form POS 1,20*C 20,20*C 8,120*N 1,20*C 12
		else
			open #hdednames=fnH: "Name=[Q]\PRmstr\DedNames.h[cno],RecL=920,use",internal,outIn,relative 
			write #hdednames,using fDedNames: mat cache_fullname$,mat cache_abrevname$,mat cache_dedcode,mat cache_calcode,mat cache_dedfed,mat cache_dedfica,mat cache_dedst,mat cache_deduc,mat cache_gl$
		end if 
		close #hdednames:
	end if

	if doWrite then
		open #hdednames=fnH: "Name=[Q]\PRmstr\DedNames.h[cno],RecL=920,use",internal,outIn,relative
		rewrite #hdednames,using fDedNames,rec=1: mat fullname$, mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$
		close #hdednames:
		doWrite=0
		goto RESTART
	end if

	mat fullname$(udim(mat cache_fullname$))
	for fullnameItem=1 to udim(mat fullname$)
		fullname$(fullnameItem)=cache_fullname$(fullnameItem) soflow ignore
	nex fullnameItem
	
	! on error goto dnFINIS !  assumes that the erroring one and all further paramters were not passed
	if fnArrayWasPassedC(mat abrevname$) then
		mat abrevname$(udim(mat cache_abrevname$))
		for abrevnameItem=1 to udim(mat abrevname$)
			abrevname$(abrevnameItem)=cache_abrevname$(abrevnameItem) soflow ignore
		nex abrevnameItem
	! else
	! 	pr 'not passed abrevname$'
	end if
	if fnArrayWasPassedN(mat dedcode) then mat dedcode(udim(mat cache_dedcode)) : mat dedcode=cache_dedcode
	if fnArrayWasPassedN(mat calcode) then mat calcode(udim(mat cache_calcode)) : mat calcode=cache_calcode
	if fnArrayWasPassedN(mat dedfed ) then mat dedfed(udim(mat cache_dedfed))   : mat dedfed=cache_dedfed
	if fnArrayWasPassedN(mat dedfica) then mat dedfica(udim(mat cache_dedfica)) : mat dedfica=cache_dedfica
	if fnArrayWasPassedN(mat dedst  ) then mat dedst(udim(mat cache_dedst))     : mat dedst=cache_dedst
	if fnArrayWasPassedN(mat deduc  ) then mat deduc(udim(mat cache_deduc))     : mat deduc=cache_deduc
	if fnArrayWasPassedC(mat gl$    ) then mat gl$(udim(mat cache_gl$))         : mat gl$=cache_gl$
	dnFINIS: !
fnend
