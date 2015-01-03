package org.savageworlds.admin.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("EdgeDescriptions")
public class EdgeDescriptionList extends ArrayList<EdgeDescriptionDto>{

	public EdgeDescriptionList(List<EdgeDescriptionDto> all) {
		super();
		addAll(all);
	}

	public EdgeDescriptionList() {
		super(); 
	}

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;
}
