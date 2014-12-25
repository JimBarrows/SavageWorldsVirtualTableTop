package org.savageworlds.admin.dto;

import java.util.ArrayList;
import java.util.List;

import org.savageworlds.game.model.ArmorDescription;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("ArmorDescriptions")
public class ArmorDescriptionList extends ArrayList<ArmorDescription> {

	public ArmorDescriptionList(List<ArmorDescription> all) {
		super();
		addAll( all);
	}

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;
}