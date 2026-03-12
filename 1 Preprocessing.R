#source("0 General.R")

print("* Preprocessing")

print("* * Behavior")
source("1.1 Behavior.R")

print("* * Questionnaires")
source("1.2 Questionnaires.R")

print("* * Eye-Tracking")
source("1.3 Eye.R")


print("* * Heart Rate")
print("* * * Trigger Check")
source("1.4.1 HR Trigger Check.R")

print("* * * Beat Detection")
source("1.4.2 HR detect beats.R")
