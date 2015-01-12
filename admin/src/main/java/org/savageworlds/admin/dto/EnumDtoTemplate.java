package org.savageworlds.admin.dto;

import java.io.Serializable;

public class EnumDtoTemplate implements Serializable{

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 1L;
	private String	id;
	private String	name;
	private Integer	sequence;

	public EnumDtoTemplate() {
		super();
	}
	
	public EnumDtoTemplate(String id, String name, int sequence) {
		this.id = id;
		this.name = name;
		this.sequence = sequence;
	}
	
	
	public EnumDtoTemplate(String id, String name, Integer sequence) {		
		this.id = id;
		this.name = name;
		this.sequence = sequence;
	}

	public Integer getSequence() {
		return sequence;
	}

	public void setSequence(Integer sequence) {
		this.sequence = sequence;
	}

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

}