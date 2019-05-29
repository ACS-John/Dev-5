! first run Get images for claims from DB3 and put them into DB1

fn_setup
on error goto Error_Hanler
fnTop(program$)
enableMoveFiles=0
dim pathFrom$*256
pathFrom$='D:\wherever'
dim imagePathFrom$*128
imagePathFrom$='F:\IMASTER2\IMAGES\CM\'
dim imagePathTo$*128
imagePathTo$  ='S:\CM\'

dim filenoList$(0)*8
mat filenoList$(0)
fnasci(env$('at')&'D:\Schachtner\claims.txt',mat filenoList$)
! fnAddOneC(mat filenoList$,'firstOne')
! fnAddOneC(mat filenoList$,'secondUn')
mat hitCount(udim(mat filenoList$))
mat hitCount=(0)

oc_first=oc_last=1 !  open only
for oc=oc_first to oc_last
	if oc=1 then ! open
		open   #hImagesTo:=fngethandle: "NAME=IMAGES.INT//6,KFNAME=IMAGES.IDX//6,shr",INTERNAL,OUTIN,KEYED
		open #hImagesFrom:=fngethandle: 'NAME='&pathFrom$&'\data\IMAGES.INT,KFNAME='&pathFrom$&'\data\IMAGES.IDX,shr',INTERNAL,OUTIN,KEYED
		! kps=1/9/13/76 kln=8/4/4/4
	else if oc=2 then ! closed
		open   #hImagesTo:=fngethandle: "NAME=IMAGES.INT//1,KFNAME=IMAGES.IDX//1,shr",INTERNAL,OUTIN,KEYED 
		open #hImagesFrom:=fngethandle: 'NAME='&pathFrom$&'\history\IMAGES.INT,KFNAME='&pathFrom$&'\history\IMAGES.IDX,shr',INTERNAL,OUTIN,KEYED
	end if
	for filenoItem=1 to udim(mat filenoList$)
		fileno$=filenoList$(filenoItem)
		restore #hImagesFrom,search=>fileno$:
		read  #hImagesFrom,using images_formall$: mat images_data$,mat images_data
		if rtrm$(images_data$(images_fileno))=rtrm$(fileno$) then
			images_data$(images_npath)=srep$(images_data$(images_npath),imagePathFrom$,imagePathTo$)
			write #hImagesTo,using images_formall$: mat images_data$,mat images_data
			hitCount(filenoItem)+=1
			if enableMoveFiles then
				pr 'move files is not yet written   [npath]\[nfile]*.*   '
				pause
			end if
				! no need to move the actual image files at this time.
			
			
		end if
	nex filenoItem
	close #hImagesFrom:
	close #hImagesTo:
nex oc
goto Xit
Xit: fnXit
def fn_setup
	if ~setup then
		setup=1
		
		library 'library\clsUtil.wb': fnget_inf_claim
		library 'library\clsUtil.wb': fnAsci
		library 'library\clsUtil.wb': fnDate_rpt10$
		library 'library\clsUtil.wb': fnAllDebtors
		! library 'library\clsUtil.wb': fnArray_to_range$
		library 'prog2\intermnt.wb': fnInternal_data


		library 'S:\Core\Library.br': fnXit
		library 'S:\Core\Library.br': fnTos
		library 'S:\Core\Library.br': fnChk
		library 'S:\Core\Library.br': fnLbl,fnTxt
		library 'S:\Core\Library.br': fnCmdSet,fnAcs
		library 'S:\Core\Library.br': fnGetHandle
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fntop

		library 'library\CLSUtil.wb': fnList_Print
		library 'library\CLSUtil.wb': fncom

		library 'Library\SQL.wb': fnsql_setup$

		dim images_data$(0)*60,images_data(0)
		dim images_fieldsc$(0)*20,images_fieldsn$(0)*20
		dim images_formall$*2048
		execute "*SubProc "&fnsql_setup$('images',mat images_data$,mat images_data,mat images_fieldsc$,mat images_fieldsn$,images_formall$)

		dim oc$(2)*6
		oc$(1)='Open'
		oc$(2)='Closed'
	end if
fnend
include: cm\err