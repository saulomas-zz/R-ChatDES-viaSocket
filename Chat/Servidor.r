source("EncodeDecode_byDES.r")
#O arquivo acima localizado na mesma pasta desse arquivo contém as funções:
#=> De geração de chave (getChaveP, getChaveQ, getChaveN, getChaveNFi, getChaveE, getChaveD)
#=> De criptografia e descriptografia (toMsgCrypt, toMsgDecrypt)

server <- function() {  
    #--------------------------------------------------------------------
    print("Aguardando Chave digitada do Cliente")

    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=TRUE, open="r+")

    # receber a chave publica do cliente
    key = readLines(con,1)
    print(paste("Chave:", key))

    close(con)
    #--------------------------------------------------------------------

    while(TRUE){
        writeLines("Escutando...")
        con = socketConnection(host="localhost", port = 777, blocking=TRUE, server=TRUE, open="r+")

        # servidor recebe mensagem enviada pelo cliente
        msgEncrypted = readLines(con, 1)
        msgEncryptedInRaw = charToRaw(msgEncrypted)

        # servidor decriptografa a mensagem e a mostra na tela
        # fazer aqui a decriptografia
        msg = msgDecrypt(msgEncryptedInRaw, key)
        print(msg)
        
        # servidor captura mensagem da entrada padrao (teclado)
        f = file("stdin")
        open(f)
        writeLines("Mensagem", sep=": ")
        msg <- readLines(f, n=1)
        if(tolower(msg)=="q"){
            break
        }
        
        # servidor criptografa a mensagem e a envia para o cliente
        # fazer aqui a criptografia
        msgEncrypted = msgEncrypt(msg, key)
        msgEncryptedInChar = rawToChar(msgEncrypted)
        write_resp <- writeLines(msgEncryptedInChar, con)

        close(con)
    }
}
server()