package quicksort

func Sort(data []int) []int {
	n := len(data)
	quicksort_r(data,0,n-1)
	return data
}

func quicksort_r(data []int, start int, end int) {
	if start < end {
		pivot := partition(data,start,end)
		quicksort_r(data,start,pivot-1)
		quicksort_r(data,pivot+1,end)
	}
}

func partition(data []int, start int, end int) int {
	pivot := start
	
	for ; start < end; {
		if data[start] <= data[end] {
			exchange(data,start,pivot)
			pivot++
		}
		start++
	}

	exchange(data,pivot,end)
	return pivot
}

func exchange(data []int, a int, b int) {
	data[a], data[b] = data[b], data[a]
}