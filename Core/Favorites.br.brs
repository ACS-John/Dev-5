fn_setup
fnTop(program$)
fnHamsterFio('CO Favorites')
Xit: fnXit
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
		dim form$(0)*256
		dim favData$(0)*128,favDataN(0)
	end if
fnend
def library fnFavoriteAdd(programCaption$*256)
	if ~setup then fn_setup
	hFav=fn_open('CO Favorites',mat favData$,mat favDataN,mat form$)
	mat favData$=('') : mat favDataN=(0)
	favData$(fav_user)=env$('Unique_Computer_ID')
	favData$(fav_system)=env$('cursys')
	favData$(fav_program)=programCaption$
	write #hFav,using form$(hFav): mat favData$,mat favDataN
	fnCloseFile(hFav,'CO Favorites')
fnend
def library fnFavoriteDel(programCaption$*256)
	if ~setup then fn_setup
	hFavProgram=fn_open('CO Favorites',mat favData$,mat favDataN,mat form$,0,2)
	mat favData$=('') : mat favDataN=(0)
	favData$(fav_user)=env$('Unique_Computer_ID')
	favData$(fav_system)=env$('cursys')
	favData$(fav_program)=programCaption$
	delete #hFavProgram,key=fnBuildKey$('CO Favorites',mat favData$,mat favDataN, 2):
	fnCloseFile(hfav,'CO Favorites')
fnend
def library fnFavoriteList(mat favorite$)
	if ~setup then fn_setup
	mat favorite$(0)
	hFav=fn_open('CO Favorites',mat favData$,mat favDataN,mat form$,1)
	restore #hFav,key>=fn_favKey$: nokey fl_eof
	do
		read #hFav,using form$(hFav):mat favData$,mat favDataN eof fl_eof
		flMatch=0
		if rtrm$(favData$(fav_user))=env$('Unique_Computer_ID') and favData$(fav_system)=env$('cursys') then
			flMatch=1
			favoriteListReturn+=1
			fnAddOneC(mat favorite$,favData$(fav_program))
		end if
	loop while flMatch
	fl_eof: !
	fnCloseFile(hFav,'CO Favorites')
	fnFavoriteList=favoriteListReturn
fnend
def fn_favKey$*42
	dim favKeyReturn$*42
	if favKeySetup$<>env$('cursys') then
		favKeySetup$=env$('cursys')
		mat favData$=('')
		mat favDataN=(0)
		favData$(fav_user)=env$('Unique_Computer_Id')
		favData$(fav_system)=env$('cursys')
		favKeyReturn$=fnBuildKey$('CO Favorites',mat favData$,mat favDataN, 1)
	end if
	fn_favKey$=favKeyReturn$
fnend
include: fn_open
include: Ertn