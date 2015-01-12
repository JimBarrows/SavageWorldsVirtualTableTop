package org.savageworlds.admin.dto;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("rankType")
public class RankTypeDto extends EnumDtoTemplate {

	public RankTypeDto(String id, String name, int ordinal) {
		super( id, name, ordinal);
	}
	

}
