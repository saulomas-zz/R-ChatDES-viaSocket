#install.packages('chunk2', repos='http://cran.us.r-project.org')

#rawToChar(packBits(rawToBits(charToRaw("64bitKey")), type = c("raw", "integer")))

source("Funcoes.r")

#inBlock = c(0,0,0,1,0,1,1,0,1,0,1,1,1,1,0,1,0,1,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,1,1,0,1,0,0,0,1,0,1,0,0,1,1,1,0,0,0,1,0,1,0,0,1,1,1,1,0,1,1)
inBlock = c(0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1)
inBlockL = chunk(inBlock, 2)[[1]]
inBlockR = chunk(inBlock, 2)[[2]]
inBlockPerm = permuteInput(inBlock, initial = TRUE)
inBlockPermL = chunk(inBlockPerm, 2)[[1]]
inBlockPermR = chunk(inBlockPerm, 2)[[2]]

#keyK = c(1,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,1,0,1,1,1,1,0,0,0,0,1,0,1,1,1,0,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,1,1,0,1,1)
keyK = c(0,0,0,1,0,0,1,1,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,0,1,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,1,1,1,1,1,0,0,0,1)
keyKPerm = permuteChoice1(keyK)

c0 = chunk(keyKPerm, 2)[[1]]
d0 = chunk(keyKPerm, 2)[[2]]

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

# for (i in 1:16) { 
#     cI = c(cI, list(sBox1l0, sBox1l1, sBox1l2, sBox1l3))

#     keyKPermLnew = shifter(keyKPermL, i) # 1º Round
#     keyKPermRnew = shifter(keyKPermR, i) # 1º Round

#     keyKPermNew = c(keyKPermLnew, keyKPermRnew)

#     k = permuteChoice2(keyKPermNew)

#     ####-------------------------------------------

#     print(paste("i =", i))
#     print(paste(c("inBlockPermR =", inBlockPermR), collapse = " "))
#     eR = permuteDbox(inBlockPermR)
    
#     print(paste(c("eR =", eR), collapse = " "))
#     B = xorB(eR, k)
    
#     bMatrix = matrix(B, 8, 6, byrow = TRUE)
#     R = funcF(bMatrix)
#     P = permutePbox(R)

#     Rnew = xorB(inBlockPermL, P)
#     Lnew = inBlockPermR
#     print(paste(c("Rnew =", Rnew), collapse = " "))
#     print(paste(c("Lnew =", Lnew), collapse = " "))
#     ####-------------------------------------TROCA

#     inBlockPermL = Lnew
#     inBlockPermR = Rnew
# }

# preOutput = c(inBlockPermR, inBlockPermL)
# preOutputMatrix = matrix(preOutput, 8, 8, byrow = TRUE)
# View(preOutputMatrix)