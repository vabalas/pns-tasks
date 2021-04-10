## 8. Užduotis

## Ištirkite automobilio variklio tūrio ir nuvažiuoto automagistralėje
## atstumo (su vienodu kiekiu degalų) sąryšį (D. C. Montgomery,
## R. C. Runger. Applied Statistics and Probability for Engineers. 3th
## ed. 2003, p. 381).

df <- data.frame(
    Brand = c(
        "Acura", "BMW", "Buick", "Chevrolet", "Chevrolet", "Chrysler",
        "Dodge", "Dodge", "Ford", "Ford", "Ford", "Ford", "Honda", "Mazda",
        "Mercedes", "Mercury", "Nissan", "Oldsmobile", "Plymouth",
        "Pontiac"
    ),
    Make = c(
        "Legend", "735i", "Regal", "Cavalier", "Celebrity", "Conquest",
        "Aries", "Dynasty", "Escort", "Mustang", "Taurus", "Tempo",
        "Accord", "RX-7", "260E", "Tracer", "Maxima", "Cutlass", "Laser",
        "GrandPrix"
    ),
    Mileage = c(
        30, 19, 29, 32, 30, 24, 30, 28, 31, 25, 27, 33, 30, 23, 24, 29, 26,
        29, 37, 29
    ),
    Displacement = c(
        97, 209, 173, 121, 151, 156, 135, 181, 114, 302, 153, 90, 119, 80,
        159, 97, 181, 173, 122, 173
    )
)
