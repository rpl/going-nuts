package mergesort

func Sort(data []int) []int {
	n := len(data)
	tmp := make([]int,n)
	mergesort_r(data,0,n-1,tmp)
	return data
}

func mergesort_r(data []int, start int, end int, tmp []int) {
	if start < end {
		mid := start + (end - start)/2
		mergesort_r(data,start,mid,tmp)
		mergesort_r(data,mid+1,end,tmp)
		merge(data,start,mid,end,tmp)
	}
}

func merge(data []int, start int, mid int, end int, tmp []int) {
	i1 := start
	i2 := mid+1
	
	for tmp_i := 0; i1 <= mid || i2 <= end; tmp_i++ {
		if i1 <= mid && i2 <= end {
			if data[i1] <= data[i2] {
				tmp[tmp_i] = data[i1]
				i1++
			} else {
				tmp[tmp_i] = data[i2]
				i2++
			}
		} else if i1 <= mid {
			tmp[tmp_i] = data[i1]
			i1++
		} else {
			tmp[tmp_i] = data[i2]
			i2++
		}
	}

	for tmp_i, data_i := 0, start; data_i <= end; tmp_i, data_i = tmp_i+1, data_i+1 {
		data[data_i] = tmp[tmp_i]
	}
}