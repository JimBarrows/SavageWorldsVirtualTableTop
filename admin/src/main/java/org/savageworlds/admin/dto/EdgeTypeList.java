package org.savageworlds.admin.dto;

import java.util.ArrayList;
import java.util.List;

import org.savageworlds.game.model.EdgeType;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("EdgeTypes")
public class EdgeTypeList extends ArrayList<EdgeType>{

	public EdgeTypeList(List<EdgeType> all) {
		super();
		addAll( all);
	}

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;
}
