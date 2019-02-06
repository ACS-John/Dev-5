! load 'S:\Collection-Master Add-On\test\fnSendEmail.br'
! library 'S:\Core\Library.br': fntop
library 'S:\Core\Library.br': fnSendEmail
! fntop(program$)
dim subject$*255
subject$='fnSendEmail Test'
dim emailMessage$*10000
emailMessage$='This is a test<br>This is line two testing<br>'
dim attachFile$*255
attachFile$=''
noPrompt=0
dim bccEmail$*255
bccEmail$=''
mat ccEmails$(0)
CCAsTo=0
fnSendEmail('niceguywinning@gmail.com',emailMessage$, subject$,attachFile$,noPrompt,bccEmail$,mat ccEmails$,CCAsTo)
end
