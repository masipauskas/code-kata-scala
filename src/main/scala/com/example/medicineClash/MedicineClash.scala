package com.example.medicineClash

import java.time.LocalDate

case class Patient(medicines: Set[Medicine])
case class Medicine(name: String, prescriptions: Set[Prescription])
case class Prescription(dispenseDate: LocalDate, daysSupply: Int)

object MedicineClash {
  def check(patient: Patient, medicineNames: Set[String], days: Int = 90)(date: LocalDate = LocalDate.now): Set[String] = {
    def between(date: LocalDate, start: LocalDate, end: LocalDate) =
      (date.isEqual(start) || date.isAfter(start)) && (date.isEqual(end) || date.isBefore(end))

    def isClashingMedicine(m: Medicine) = medicineNames.contains(m.name)
    def hasActivePrescription(m: Medicine)(start: LocalDate, end: LocalDate) = {
      m.prescriptions.exists { p =>
        val prescriptionStartDate = p.dispenseDate
        val prescriptionEndDate = prescriptionStartDate.plusDays(p.daysSupply)

        (between(prescriptionStartDate, start, end)
          || between(prescriptionEndDate, start, end)
          || (prescriptionStartDate.isBefore(start) && prescriptionEndDate.isAfter(end)))
      }
    }

    val startDate = date.minusDays(days)
    val endDate = date

    patient.medicines
      .filter(isClashingMedicine)
      .filter(hasActivePrescription(_)(startDate, endDate))
      .map(_.name)
  }
}
