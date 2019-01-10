df <- read.csv("complete_clean_data.csv")
colnames(df)
head(df)
DUI <- table(df$driver_race, df$DUIviolation)
DUItest <- chisq.test(DUI)
DUItest$observed
DUItest$expected
 
Speed <- table(df$driver_race, df$SpeedingViolation)
Speedtest <- chisq.test(Speed)
Speedtest$observed
Speedtest$expected

Equipment <- table(df$driver_race, df$EquipmentViolation)
Equipmenttest <- chisq.test(Equipment)
Equipmenttest$observed
Equipmenttest$expected


License <- table(df$driver_race, df$LicenseViolation)
Licensetest <- chisq.test(License)
Licensetest$observed
Licensetest$expected

Lights <- table(df$driver_race, df$LightsViolation)
Lightstest <- chisq.test(Lights)
Lightstest$observed
Lightstest$expected

Paperwork <- table(df$driver_race, df$PaperworkViolation)
Paperworktest <- chisq.test(Paperwork)
Paperworktest$observed
Paperworktest$expected

SafeMovement <- table(df$driver_race, df$SafeMovementViolation)
SafeMovementtest <- chisq.test(SafeMovement)
SafeMovementtest$observed
SafeMovementtest$expected

Stop <- table(df$driver_race, df$StoppingViolation)
Stoptest <- chisq.test(Stop)
Stoptest$observed
Stoptest$expected

Registration <- table(df$driver_race, df$RegistrationViolation)
Registrationtest <- chisq.test(Registration)
Registrationtest$observed
Registrationtest$expected

SeatBelt <- table(df$driver_race, df$SeatBeltViolation)
SeatBelttest <- chisq.test(SeatBelt)
SeatBelttest$observed
SeatBelttest$expected

Moving <- table(df$driver_race, df$MovingViolation)
Movingtest <- chisq.test(Moving)
Movingtest$observed
Movingtest$expected

CellPhone <- table(df$driver_race, df$CellPhoneViolation)
CellPhonetest <- chisq.test(CellPhone)
CellPhonetest$observed
CellPhonetest$expected

Truck <- table(df$driver_race, df$TruckViolation)
Trucktest <- chisq.test(Truck)
Trucktest$observed
Trucktest$expected

Other <- table(df$driver_race, df$OtherViolation)
Othertest <- chisq.test(Other)
Othertest$observed
Othertest$expected
Othertest$residuals

Arrest <- table(df$driver_race, df$ArrestOrCitation)
Arresttest <- chisq.test(Arrest)
Arresttest$observed
Arresttest$expected