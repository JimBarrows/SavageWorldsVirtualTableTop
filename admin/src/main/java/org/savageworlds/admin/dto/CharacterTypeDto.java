package org.savageworlds.admin.dto;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("characterType")
public class CharacterTypeDto extends EnumDtoTemplate{
	public CharacterTypeDto(String id, String name, int ordinal) {
		super( id, name, ordinal);
	}
}
