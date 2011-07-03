package heapsort

func Sort(data []int) []int {
	heapsort(data)
	return data
}

func heapsort(data []int) {
	n := len(data)
	
	for k := n/2; k>=0; k-- {
		sink(data,k,n-1)		
	}

	for n>0 {
		n--
		exch(data,0,n)
		sink(data,0,n-1)
	}
}

func sink(data []int, k int, end int) {
	for 2*k<=end {
		i := 2*k
		if i<end && data[i] < data[i+1] {
			i++
		}
		if data[k] >= data[i] {
			break
		}
		exch(data,k,i)
		k = i
	}
}

func exch(data []int, a int, b int) {
	data[a],data[b]=data[b],data[a]
}