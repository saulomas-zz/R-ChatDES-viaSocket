#install.packages('chunk2', repos='http://cran.us.r-project.org')

#rawToChar(packBits(rawToBits(charToRaw("64bitKey")), type = c("raw", "integer")))

source("Funcoes.r")

#inBlock = c(0,0,0,1,0,1,1,0,1,0,1,1,1,1,0,1,0,1,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,1,1,0,1,0,0,0,1,0,1,0,0,1,1,1,0,0,0,1,0,1,0,0,1,1,1,1,0,1,1)
#M = 0123456789ABCDEF
inBlock = c(0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1)
inBlockPerm = permute(inBlock, initial = TRUE)
inBlockPermL = chunk(inBlockPerm, 2)[[1]]
inBlockPermR = chunk(inBlockPerm, 2)[[2]]

#keyK = c(1,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,1,0,1,1,1,0,1,1,1,0,0,1,1,0,1,1,1,1,0,0,0,0,1,0,1,1,1,0,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,1,1,0,1,1)
#K = 133457799BBCDFF1
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

rm(i)
rm(c, cTemp, d, dTemp, k)

l0 = inBlockPermL
r0 = inBlockPermR

lTemp = l0
rTemp = r0

lI = list()
rI = list()

for(i in 1:16) {
    l = rTemp
    #r = lTemp

    e_rTemp = permuteDbox(rTemp)    
    resultXOR = xorBit(e_rTemp, kI[[i]])

    resultXORmatrix = matrix(resultXOR, 8, 6, byrow = TRUE)

    outSbox = resultXORBySbox(resultXORmatrix)
    outSboxPerm = permutePbox(outSbox)

    r = xorBit(lTemp, outSboxPerm)

    lI = c(lI, list(l))
    rI = c(rI, list(r))
    
    rTemp = r
    lTemp = l
}

l16r16inverted = c(rI[[16]],lI[[16]])

l16r16invertedPerm = permute(l16r16inverted, initial = FALSE)

# M = 0123456789ABCDEF
# inBlock = c(0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1)

# K = 133457799BBCDFF1
# keyK = c(0,0,0,1,0,0,1,1,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,0,1,1,0,1,1,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,1,1,1,1,1,0,0,0,1)

rawToChar(packBits(rawToBits(charToRaw("64bitKey")), type = c("raw", "integer")))

rm(i)

# preOutput = c(inBlockPermR, inBlockPermL)
# preOutputMatrix = matrix(preOutput, 8, 8, byrow = TRUE)
# View(preOutputMatrix)