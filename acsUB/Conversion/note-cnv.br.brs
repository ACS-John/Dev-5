! Replace S:\acsUB\conversion\Note-cnv
! this program converts the footnotes to new format
def library fnub_cnv_note
	library 'S:\Core\Library': fnerror,fnStatus,fnCopy,fnub_cnv_note_phase_1,fngethandle,fnindex_it
	if exists("[Q]\UBmstr\Note1.h[cno]") then
		fnub_cnv_note_phase_1 ! this needs to be done first in case they are on version 1 on note1 and note2 
		fnStatus('Converting Notes (S:\acsUB\conversion\Note-cnv)')
		dim nam$*30,rm$*60,ra(2),z$*10

		fnindex_it("[Q]\UBmstr\Note1.h[cno]", "[Q]\UBmstr\NoteIdx1.h[cno]","1 10")
		open #h_note1:=fngethandle: "Name=[Q]\UBmstr\Note1.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno],Shr",internal,input,keyed 
		!   fnCopy("[Q]\UBmstr\Note2.h[cno]","[Q]\UBmstr\Note2.h[cno]",73)  ! BAD IDEA - THIS PROGRAM DOES NOT HANDLE THAT FORMAT OF NOTE2.hxx file - I think S:\acsUB\Conversion\note-cnv-c7.br will convert it to the format necessary for this to continue though

		open #h_note2:=fngethandle: "Name=[Q]\UBmstr\Note2.h[cno],Shr",internal,outIn,relative
		if kln(h_note2)<73 then
			close #h_note2:
			fnCopy("[Q]\UBmstr\Note2.h[cno]","[Q]\UBmstr\Note2.h[cno]", 73)
			open #h_note2:=fngethandle: "Name=[Q]\UBmstr\Note2.h[cno],Shr,Use,RecL=73",internal,outIn,relative 
		end if

		do
			read #h_note1,using 'form pos 1,c 10,2*pd 3': z$,mat ra eof DONE
			if ~exists("[Q]\UBmstr\notes.h[cno]") then execute "mkdir [Q]\UBmstr\notes.h[cno]"
			adr=ra(1)
			if adr<>0 then 
				do 
					read #h_note2,using 'form pos 1,c 10,c 60,pd 3',rec=adr: k32$,rm$,adr noRec NextNote1
					open #hNote33:=fngethandle: "Name=[Q]\UBmstr\notes.h[cno]\"&trim$(z$)&".txt,RecL=128,use",display,output  ! XXX changed from ,Replace to ,Use on 5/4/2018
					pr #hNote33: rm$
					close #hNote33:
					delete #h_note2:
				loop until adr<=0
			end if
			NextNote1: ! 
		loop
		DONE: ! 
		close #h_note2:
		close #h_note1:
		execute 'free [Q]\UBmstr\Note1.h[cno]' ioerr ignore
		execute 'free [Q]\UBmstr\Note2.h[cno]' ioerr ignore
		fnStatus('Note Conversion of company number [cno] completed successfully')
	end if
	XIT: !
fnend
!
include: ertn