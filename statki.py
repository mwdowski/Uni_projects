import random

def utworz_plansze(n, maxiter):
    """
    Funkcja tworzaca plansze o wymiarach /n x n/, ktora generuje losowo statki przy /maxiter/ probach
    Statki sa ustawione tylko pionowo
    """
    assert n >= 4
    #plansza
    P = [[0]*n for i in range(n)]
    for i in range(maxiter):
        #dlugosc statku
        dl = random.randint(1, 4)
        
        #wspolrzedna pozioma poczatku statku
        y = random.randint(0, n-1)
        #wspolrzedna pionowa statku
        x = random.randint(0, n-dl)
        
        def jest_miejsce(x, y, dl, P):
            """
            Funkcja sprawdza, czy strefa wokol statku, ktory chcemy stworzyc, jest wolna
            """
            for i in range(y-1, y+2):
                for j in range(x-1, x+dl+1):
                    #sprawdzamy, czy nie przekraczamy zakresu
                    if j>=0 and j<len(P) and i>=0 and i<len(P):
                        if P[j][i] != 0: return False
            return True
        
        #jesli warunek jest spelniony, funkcja wylosuje nowe miesjce dla statku
        if not jest_miejsce(x, y, dl, P): continue
        
        #rysowanie prawidlowo wygenerowanego statku
        for i in range(dl):
            P[x+i][y] = 1
        
    return P

def odkryj_pole(x, y, P):
    """
    Odkrywa pole o podanych wspolrzednych
    """
    if P[x][y]==0: P[x][y]=2
    if P[x][y]==1: P[x][y]=3
    return

def czy_wszystkie_trafione(P):
    """
    Sprawdza, czy zostaly pola =1, czyli z nieznalezionym statkiem
    """
    for i in range(len(P)):
        for j in range(len(P)):
            if P[i][j]==1: return False
    return True

def wypisz(P):
    n = len(P)
    # wypisanie numerow kolumn planszy
    for k in range(n):
        if k == 0: print(str.format("{0:^4s}", " "), end="")
        print(str.format("{0:^3s}", str(k)), end="")
    print("\n"+"-"*(n*3+4))
    
    
    for i in range(n):
        for j in range(n):
            # wypisanie numerow wierszy planszy
            if j == 0: print(str.format("{0:^3s}|", str(i)), end="") 
            
            # wypisanie pol:
            # ' ' gdy pole nie jest odkryte
            if P[i][j] == 0 or P[i][j]==1: print("   ", end="")
            # 'o' pole odkryte bez statku
            if P[i][j] == 2: print(" o ", end="")
            # 'x' pole odkryte zawierajace fragment statku
            if P[i][j] == 3: print(" x ", end="")
        print("")    
    print("\n")

def gra(n = 10, maxiter = 10):
    """
    Wlasciwa gra - sprawda pole na wygenerowanej planszy i sporawdza, czy wszytskie statki zostaly juz zatopione
    """
    P = utworz_plansze(n, maxiter)
    zl = 0
    wypisz(P)
    while not czy_wszystkie_trafione(P):
        x = int(input("Podaj wspolrzedna pionowa pola: "))
        y = int(input("Podaj wspolrzedna pozioma pola: "))
        if x>=n or y>=n or x<0 or y<0:
            print("Nieprawidlowe pole!")
            continue
        zl+=1
        odkryj_pole(x, y, P)
        wypisz(P)
    print("Gratulacje! Zajelo ci to ", zl, " prob!", sep="")

#main:
print("Witaj w grze w statki!\n")
gra()