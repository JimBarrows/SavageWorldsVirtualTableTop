package org.savageworlds.admin.dto;

import java.util.ArrayList;

import org.savageworlds.game.model.SkillDescription;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("SkillDescriptions")
public class SkillDescriptionList extends ArrayList<SkillDescription> {

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;

}
