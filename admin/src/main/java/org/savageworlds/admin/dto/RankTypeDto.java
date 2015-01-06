package org.savageworlds.admin.dto;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("rankType")
public class RankTypeDto {

	private String	id;
	private String	name;

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public RankTypeDto(String id, String name) {
		super();
		this.id = id;
		this.name = name;
	}

	public RankTypeDto() {
		super();
		// TODO Auto-generated constructor stub
	}

}
