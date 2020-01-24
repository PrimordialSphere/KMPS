package main

import (
	"fmt"
	"sync"
	"time"
)

type person struct {
	steps       int
	start       int
	destination int
	getMeHome   chan person
}

func steuerungEinheit(numPersons int, cp *sync.WaitGroup) []person {
	var myPersonenArray []person
	personsJobs := make(chan person, numPersons)
	done := make(chan bool)
	stopchan := make(chan struct{})
	personenFahrstuhlAnmeldungsChannel := make(chan person)
	go fahrstuhlSteuerung(personenFahrstuhlAnmeldungsChannel, stopchan, numPersons)
	for i := 0; i < numPersons; i++ {
		var myChannel = make(chan person)
		var myPerson = person{1, 1, i, myChannel}
		go imPerson(i, personsJobs, personenFahrstuhlAnmeldungsChannel, done, myPerson)
	}
	for finishedPersons := range personsJobs {
		fmt.Println("I returned to my Origin", finishedPersons)
		myPersonenArray = append(myPersonenArray, finishedPersons)
	}

	close(personsJobs)

	for j := 0; j < numPersons; j++ {
		<-done
	}

	close(stopchan)
	defer cp.Done()
	return myPersonenArray

}

func imPerson(id int, personenJobs chan person, personenFahrstuhlAnmeldungsChannel chan person, done chan bool, myPerson person) {

	fmt.Println(id, "Ich wurde erfolgreich erzeugt", myPerson)

	personenFahrstuhlAnmeldungsChannel <- myPerson

	for {

		returnedPerson, more := <-myPerson.getMeHome
		if more {
			fmt.Println("Ich bin auf dem Weg nach Hause")
			personenJobs <- returnedPerson
		} else {
			done <- true
			return
		}
	}

}

func fahrstuhlSteuerung(personenFahrstuhlAnmeldungsChannel chan person, stopchan chan struct{}, numPersons int) {

	for {
		select {
		case newPerson := <-personenFahrstuhlAnmeldungsChannel:
			var returnThis = newPerson.getMeHome
			returnThis <- newPerson

		case <-stopchan:
			fmt.Println("I'm getting closed")
			return
		default:

		}
	}
}

func main() {
	var numPersons = 10
	var myPersonenArray []person
	var cp sync.WaitGroup
	var averageStepsWaited int
	cp.Add(1)

	myPersonenArray = steuerungEinheit(numPersons, &cp)
	cp.Wait()

	for _, num := range myPersonenArray {
		averageStepsWaited += num.steps
	}
	averageStepsWaited = averageStepsWaited / numPersons

	fmt.Println(averageStepsWaited)
	fmt.Println("test")

	time.Sleep(10 * time.Second)

}
