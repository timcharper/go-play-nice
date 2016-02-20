package gofunc

type Address struct {
	houseNumber int64
	street      string
	city        string
	state       string
}

type Person struct {
	name string
	addr Address
}

func FormatPerson(p Person) string {
	return (p.name + " lives at " +
		string(p.addr.houseNumber) +
		" " +
		p.addr.street + ", " +
		p.addr.city + " " + p.addr.state)
}
