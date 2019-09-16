source("Funcoes.r")

encodeDecode = function(inputTextInBitsNum, key, encode = TRUE) {
    inputTextInBitsNumPerm = permute(inputTextInBitsNum, initial = TRUE)

    # inputKeyInBitsNum = c(0,0,1,1,0,1,1,0,0,0,1,1,0,1,0,0,0,1,1,0,0,0,1,0,0,1,1,0,1,0,0,1,0,1,1,1,0,1,0,0,0,1,0,0,1,0,1,1,0,1,1,0,0,1,0,1,0,1,1,1,1,0,0,1)
    inputKeyInBitsNum = keyInBitsNum(key)
    inputKeyInBitsNumPerm = permuteChoice1(inputKeyInBitsNum)

    c0 = chunk(inputKeyInBitsNumPerm, 2)[[1]]
    d0 = chunk(inputKeyInBitsNumPerm, 2)[[2]]

    cTemp = c0
    dTemp = d0

    cI = list()
    dI = list()

    for (i in 1:16) {
        c = shifter(cTemp, i)
        d = shifter(dTemp, i)

        cI = c(cI, list(c))
        dI = c(dI, list(d))

        cTemp = c
        dTemp = d
    }

    kI = list()
    for (i in 1:16) {
        k = permuteChoice2(c(cI[[i]],dI[[i]]))

        kI = c(kI, list(k))
    }

    rm(i)
    rm(c, cTemp, d, dTemp, k)

    l0 = chunk(inputTextInBitsNumPerm, 2)[[1]]
    r0 = chunk(inputTextInBitsNumPerm, 2)[[2]]

    lTemp = l0
    rTemp = r0

    lI = list()
    rI = list()

    if (encode) {
        for(i in 1:16) {
            l = rTemp
            r = xorBit(lTemp, funcF(rTemp, kI[[i]]))

            lI = c(lI, list(l))
            rI = c(rI, list(r))
            
            rTemp = r
            lTemp = l
        }
    } else {
        for(i in 16:1) {
            l = rTemp
            r = xorBit(lTemp, funcF(rTemp, kI[[i]]))

            lI = c(lI, list(l))
            rI = c(rI, list(r))
            
            rTemp = r
            lTemp = l
        }
    }

    rm(i)
    rm(lTemp, rTemp, l, r)

    l16r16inverted = c(rI[[16]],lI[[16]])

    l16r16invertedPerm = permute(l16r16inverted, initial = FALSE)

    l16r16invertedPermRaw = as.raw(l16r16invertedPerm)
    # View(matrix(l16r16invertedPerm, 8, 8, byrow = TRUE))
    
    if (encode) {
        return(packBits(l16r16invertedPermRaw, type = c("raw", "integer")))
    } else {
        return(rawToChar(packBits(l16r16invertedPermRaw, type = c("raw", "integer"))))
    }
}

keyInBitsNum = function(key) {
    keyInRaw = charToRaw(key)

    while (length(keyInRaw) %% 8 != 0) {
        keyInRaw = c(keyInRaw, raw(1))
    }
    keyInBits = rawToBits(keyInRaw)

    return(as.numeric(keyInBits))
}

msgEncrypt = function(msg, key) {
    msgInRaw = charToRaw(msg)
    inputTextInRaw = msgInRaw

    while (length(inputTextInRaw) %% 16 != 0) {
        inputTextInRaw = c(inputTextInRaw, raw(1))
    }
    inputTextInBits = rawToBits(inputTextInRaw)
    inputTextInBitsNum = as.numeric(inputTextInBits)
    
    numBlocks = length(inputTextInBitsNum) / 64
    blocksInMatrix = matrix(inputTextInBitsNum, numBlocks, 64, byrow = TRUE)

    msgCiphered = NULL 
    for(i in 1:numBlocks) {
        msgCiphered = c(msgCiphered, encodeDecode(blocksInMatrix[i,], key, encode = TRUE))
    }

    return(msgCiphered)
}

msgDecrypt = function(msg, key) {
    msgInBits = rawToBits(msg)
    msgInBitsNum = as.numeric(msgInBits)
    
    numBlocks = length(msgInBitsNum) / 64
    blocksInMatrix = matrix(msgInBitsNum, numBlocks, 64, byrow = TRUE)

    msgDeciphered = NULL 
    for(i in 1:numBlocks) {
        msgDeciphered = c(msgDeciphered, encodeDecode(blocksInMatrix[i,], key, encode = FALSE))
    }

    return(paste(msgDeciphered, collapse = ""))
}