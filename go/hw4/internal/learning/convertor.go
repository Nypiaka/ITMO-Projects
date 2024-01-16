package learning

func fromSubject(s []subjectInfo) []string {
	result := make([]string, len(s))
	for i := range s {
		result[i] = s[i].name
	}
	return result
}
