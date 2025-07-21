(setq smtpmail-smtp-server "smtp.gmail.com"
      smptmail-smtp-service 465
      smptmail-stream-type 'ssl)
(setq message-send-mail-function 'smtpmail-send-it)
