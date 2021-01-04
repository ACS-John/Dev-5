autoLibrary
fnTop(program$)
dim theFile$*128
theFile$='common\clssetup'
dim backupFile$*128
backupFile$=theFile$&'-'&date$('ccyymmdd')&'.bak'
exe 'cop "'&theFile$&'" "'&backupFile$&'"'
pr 'If backup worked type GO and press Enter to continue else STOP and SYS' : pau
ope #h=fnH: 'name='&theFile$&',shr',i,outin,relative
rea #h,u 'form pos 1,x 54,c 8',rec=2: theDate$
pr 'before: '&theDate$
theDate$=date$(days(theDate$,'ccyymmdd')+365,'ccyymmdd')
rew #h,u 'form pos 1,x 54,c 8',rec=2: theDate$
pr ' after: '&theDate$
pr 'type GO and press Enter to continue' : pau
cl #h: 
fnXit