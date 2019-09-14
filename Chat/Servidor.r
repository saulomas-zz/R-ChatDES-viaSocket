source("Util.r")
#O arquivo acima localizado na mesma pasta desse arquivo contém as funções:
#=> De geração de chave (getChaveP, getChaveQ, getChaveN, getChaveNFi, getChaveE, getChaveD)
#=> De criptografia e descriptografia (toMsgCrypt, toMsgDecrypt)

server <- function() {
    #--------------------------------------------------------------------
    #Geração das Chaves Públicas e Privadas
    print("Geracao da Chave Publica")
    serv_p = getChaveP()
    serv_q = getChaveQ(serv_p)
    serv_n = getChaveN(serv_p, serv_q)
    serv_n_fi = getChaveNFi(serv_p, serv_q)
    serv_e = getChaveE(serv_n_fi)
    serv_d = getChaveD(serv_n_fi, serv_e)    
    #--------------------------------------------------------------------
    print("Troca de chaves")
    #kpus = c(serv_n,serv_e)
    kpus = paste(c(serv_n, serv_e), collapse = ",")
    con <- socketConnection(host="localhost", port=666, blocking=TRUE, server=TRUE, open="r+")

    # receber a chave publica do cliente
    kpuc = readLines(con,1)
    print(kpuc)

    # enviar a chave publica do servidor
    write_resp = writeLines(kpus, con)

    cli_n = as.integer(unlist(strsplit(kpuc, split=",")))[1]
    cli_e = as.integer(unlist(strsplit(kpuc, split=",")))[2]

    close(con)
    #--------------------------------------------------------------------    
    rm(kpuc)
    rm(kpus)
    #--------------------------------------------------------------------

    while(TRUE){
        writeLines("Listening...")
        con = socketConnection(host="localhost", port = 666, blocking=TRUE, server=TRUE, open="r+")

        # servidor recebe mensagem enviada pelo cliente
        msgCrypt = readLines(con, 1)

        # servidor decriptografa a mensagem e a mostra na tela
        # fazer aqui a decriptografia
        msg = toMsgDecrypt(msgCrypt, serv_d, serv_n)
        print(msg)
        
        # servidor captura mensagem da entrada padrao (teclado)
        f = file("stdin")
        open(f)
        writeLines("msg", sep=": ")
        msg <- readLines(f, n=1)
        if(tolower(msg)=="q"){
            break
        }
        
        # servidor criptografa a mensagem e a envia para o cliente
        # fazer aqui a criptografia
        msgCrypt = toMsgCrypt(msg, cli_e, cli_n)
        write_resp <- writeLines(msgCrypt, con)

        close(con)
    }
}
server()