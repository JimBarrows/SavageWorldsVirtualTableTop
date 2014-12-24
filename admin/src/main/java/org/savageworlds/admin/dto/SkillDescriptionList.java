package org.savageworlds.admin.dto;

import java.util.ArrayList;
import java.util.List;

import org.savageworlds.game.model.SkillDescription;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("SkillDescriptions")
public class SkillDescriptionList extends ArrayList<SkillDescription> {

	public SkillDescriptionList(List<SkillDescription> all) {
		super();
		addAll( all);
	}

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;

}
