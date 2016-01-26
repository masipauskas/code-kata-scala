package com.example.medicineClash

import java.time.LocalDate

import org.scalatest.{FlatSpec, Matchers}

class MedicineClashSpec extends FlatSpec with Matchers {
  import MedicineClash.check
  val patient = Patient(Set(
    Medicine("Aspirine", Set(
      Prescription(LocalDate.of(2016, 1, 1), 30),
      Prescription(LocalDate.of(2015, 12, 1), 45),
      Prescription(LocalDate.of(2017, 1, 1), 120)
    )),
    Medicine("Analgine", Set(
      Prescription(LocalDate.of(2015, 11, 15), 90),
      Prescription(LocalDate.of(2014, 1, 1), 15)
    ))
    )
  )

  "check" should "return no medicines, if no clash exist due to clash expired" in {
    check(patient, Set("Analgin"), 30)(LocalDate.of(2016, 1, 1)) shouldEqual Set()
  }

  "check" should "return no medicines, if no clash exist due to clash being in the future" in {
    check(patient, Set("Aspirine"), 30)(LocalDate.of(2016, 6, 1)) shouldEqual Set()
  }

  "check" should "return no medicines, if clash exist between prescriptions" in {
    check(patient, Set("Analgine"), 30)(LocalDate.of(2014, 6, 15)) shouldEqual Set()
  }

  "check" should "return Analgine, if clash exist in the past" in {
    check(patient, Set("Analgine"), 30)(LocalDate.of(2014, 1, 15)) shouldEqual Set("Analgine")
  }

  "check" should "return Analgine and Aspirine, if clash exist currently" in {
    check(patient, Set("Aspirine", "Analgine"), 30)(LocalDate.of(2016, 1, 1)) shouldEqual Set("Analgine", "Aspirine")
  }
}
