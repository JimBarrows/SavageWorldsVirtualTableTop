package org.savageworlds.admin.dto;

import java.util.ArrayList;
import java.util.Collection;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("errors")
public class ErrorList<T> extends ArrayList<T> {

	public ErrorList(Collection<? extends T> c) {
		addAll(c);
	}

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;

}
