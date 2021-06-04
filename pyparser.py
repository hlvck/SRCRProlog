import csv
import unidecode
from math import sqrt

def fixline(ponto):
    ponto["PONTO_RECOLHA_FREGUESIA"] = unidecode.unidecode(ponto["PONTO_RECOLHA_FREGUESIA"])
    ponto["PONTO_RECOLHA_LOCAL"] = unidecode.unidecode(ponto["PONTO_RECOLHA_LOCAL"])
    ponto["CONTENTOR_TIPO"] = unidecode.unidecode(ponto["CONTENTOR_TIPO"])

def pontobuilder(ponto):
    localatual = "'" + ponto["PONTO_RECOLHA_LOCAL"][6:].split(':')[0][1:] + "'"
    pontostr = (ponto["PONTO_RECOLHA_LOCAL"][0:5], ponto["Latitude"], ponto["Longitude"], "'"+ponto["PONTO_RECOLHA_FREGUESIA"]+"'", localatual, "['"+ponto["CONTENTOR_RESÍDUO"].replace(",","','")+"']", ponto["CONTENTOR_TOTAL_LITROS"])
    return ",".join(pontostr)

def distance(lat1, lon1, lat2, lon2):
    return round(sqrt((lat2-lat1)*(lat2-lat1) + (lon2-lon1)*(lon2-lon1))*100000)

def parsePontos():
    csvfile = open("dataset.csv", "r")
    pontos = open("pontos.pl", "w")
    adjf = open("adjacencias.pl", "w")
    reader = csv.DictReader(csvfile)
    pontoLocalID = ""
    pontoNum = 0
    totalCap = 0
    adjacencyTracker = []
    pontoNameList = []

    for row in reader:
        fixline(row)
        novoPontoID = row["PONTO_RECOLHA_LOCAL"][0:5]
        if(novoPontoID != pontoLocalID):
            if(pontoNum > 0):
                currentPonto["CONTENTOR_TOTAL_LITROS"] = str(totalCap)
                pontos.write("ponto(" + pontobuilder(currentPonto) + ").\n")
                adjf.write("adjacencia(" + pontoLocalID +","+ novoPontoID + "," + str(distance(
                   float(currentPonto["Latitude"]), float(currentPonto["Longitude"]),
                   float(row["Latitude"]), float(row["Longitude"]))) + ").\n")

            totalCap = int(row["CONTENTOR_TOTAL_LITROS"])
            currentPonto = row
            pontoNameList.append((novoPontoID, row["PONTO_RECOLHA_LOCAL"][6:].split(':')[0][1:], row["Latitude"], row["Longitude"]))
            pontoLocalID = novoPontoID
            pontoNum += 1
            try:
                extras = row["PONTO_RECOLHA_LOCAL"][6:].split(':')[1][1:-1].split(" - ")
                adjacencyTracker.append((novoPontoID, extras, row["Latitude"], row["Longitude"]))
            except:
                continue
        else:
            if(row["CONTENTOR_RESÍDUO"] not in currentPonto["CONTENTOR_RESÍDUO"]):
                currentPonto["CONTENTOR_RESÍDUO"] = currentPonto["CONTENTOR_RESÍDUO"] + "," + row["CONTENTOR_RESÍDUO"]
            totalCap += int(row["CONTENTOR_TOTAL_LITROS"])


    currentPonto["CONTENTOR_TOTAL_LITROS"] = str(totalCap)
    pontos.write("ponto(" + pontobuilder(currentPonto) + ").\n")

    for adjacency in adjacencyTracker:
        for adjs in adjacency[1]:
            for pnt in pontoNameList:
                if adjs in pnt[1] and pnt[0] != adjacency[0]:
                    adjf.write("adjacencia(" + adjacency[0] +","+ pnt[0] + "," + str(distance(
                    float(pnt[2]), float(pnt[3]),
                    float(adjacency[2]), float(adjacency[3]))) + ").\n")

parsePontos()