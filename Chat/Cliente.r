source("Util.r")
#O arquivo acima localizado na mesma pasta desse arquivo contém as funções:
#=> De geração de chave (getChaveP, getChaveQ, getChaveN, getChaveNFi, getChaveE, getChaveD)
#=> De criptografia e descriptografia (toMsgCrypt, toMsgDecrypt)

client <- function() {
    #--------------------------------------------------------------------
    #Geração das Chaves Públicas e Privadas    
    print("Geracao da Chave Publica")
    cli_p = getChaveP()
    cli_q = getChaveQ(cli_p)
    cli_n = getChaveN(cli_p, cli_q)
    cli_n_fi = getChaveNFi(cli_p, cli_q)
    cli_e = getChaveE(cli_n_fi)
    cli_d = getChaveD(cli_n_fi, cli_e)    
    #--------------------------------------------------------------------
    print("Troca de chaves")
    #kpuc = c(cli_n,cli_e)
    kpuc = paste(c(cli_n, cli_e), collapse = ",")
    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=FALSE, open="r+")

    # enviar a chave publica do cliente
    write_resp = writeLines(kpuc, con)

    # receber a chave publica do servidor
    kpus = readLines(con,1)
    print(kpus)    

    serv_n = as.integer(unlist(strsplit(kpus, split=",")))[1]
    serv_e = as.integer(unlist(strsplit(kpus, split=",")))[2]

    close(con)
    #--------------------------------------------------------------------
    rm(kpuc)
    rm(kpus)
    #--------------------------------------------------------------------

    while(TRUE){
        con = socketConnection(host="localhost", port = 666, blocking=TRUE, server=FALSE, open="r+")

        # cliente captura mensagem da entrada padrao (teclado)
        f <- file("stdin")
        open(f)
        writeLines("msg", sep=": ")
        msg <- readLines(f, n=1)
        if(tolower(msg)=="q"){
            break
        }
        
        # cliente criptografa a mensagem e a envia para o servidor
        msgCrypt = toMsgCrypt(msg, serv_e, serv_n)
        write_resp = writeLines(msgCrypt, con)

        # cliente recebe mensagem enviada pelo servidor
        msgCrypt = readLines(con, 1)
        
        # cliente decriptografa a mensagem e a mostra na tela
        # fazer aqui a decriptografia
        msg = toMsgDecrypt(msgCrypt, cli_d, cli_n)
        print(msg)

        close(con)    
    }
}
client()