package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Positive;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

@Entity
public class PlotPoint {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name;

	private String description;

	@Positive
	@Min(1)
	private int maximumMinorHindrances = 2;

	@Positive
	@Min(1)
	private int maximumMajorHindrances = 1;

	@Positive
	@Min(1)
	private int maximumAttributePoints = 5;

	@Positive
	@Min(1)
	private int maximumSkillPoints = 15;

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Ammunition> ammunition = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<ArcaneBackground> arcaneBackgrounds = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Beast> beasts = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Edge> edges = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Gear> gear = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<GearCategory> gearCategories = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<GearType> gearTypes = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Hindrance> hindrances = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Location> locations = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Persona> personas = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Power> powers = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Race> races = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<SettingRule> settingRules = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Skill> skills = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Story> stories = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<Vehicle> vehicles = new LinkedHashSet<>();

	@OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
	@OrderBy("name")
	private Set<VehicleType> vehicleTypes = new LinkedHashSet<>();

	public Set<Ammunition> getAmmunition() {
		return ammunition;
	}

	public void setAmmunition(final Set<Ammunition> ammunition) {
		this.ammunition = ammunition;
	}

	public Set<GearCategory> getGearCategories() {
		return gearCategories;
	}

	public void setGearCategories(final Set<GearCategory> gearCategories) {
		this.gearCategories = gearCategories;
	}

	public Set<GearType> getGearTypes() {
		return gearTypes;
	}

	public void setGearTypes(final Set<GearType> gearTypes) {
		this.gearTypes = gearTypes;
	}

	public Set<VehicleType> getVehicleTypes() {
		return vehicleTypes;
	}

	public void setVehicleTypes(final Set<VehicleType> vehicleTypes) {
		this.vehicleTypes = vehicleTypes;
	}

	public Set<Story> getStories() {
		return stories;
	}

	public void setStories(final Set<Story> stories) {
		this.stories = stories;
	}

	public int getMaximumMinorHindrances() {
		return maximumMinorHindrances;
	}

	public void setMaximumMinorHindrances(final int maximumMinorHindrances) {
		this.maximumMinorHindrances = maximumMinorHindrances;
	}

	public int getMaximumMajorHindrances() {
		return maximumMajorHindrances;
	}

	public void setMaximumMajorHindrances(final int maximumMajorHindrances) {
		this.maximumMajorHindrances = maximumMajorHindrances;
	}

	public int getMaximumAttributePoints() {
		return maximumAttributePoints;
	}

	public void setMaximumAttributePoints(final int maximumAttributePoints) {
		this.maximumAttributePoints = maximumAttributePoints;
	}

	public int getMaximumSkillPoints() {
		return maximumSkillPoints;
	}

	public void setMaximumSkillPoints(final int maximumSkillPoints) {
		this.maximumSkillPoints = maximumSkillPoints;
	}

	public Set<Beast> getBeasts() {
		return beasts;
	}

	public void setBeasts(final Set<Beast> beasts) {
		this.beasts = beasts;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof PlotPoint)) return false;
		final PlotPoint plotPoint = (PlotPoint) o;
		return getId() == plotPoint.getId() &&
				getVersion() == plotPoint.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("name", name)
				.append("description", description)
				.append("maximumMinorHindrances", maximumMinorHindrances)
				.append("maximumMajorHindrances", maximumMajorHindrances)
				.append("maximumAttributePoints", maximumAttributePoints)
				.append("maximumSkillPoints", maximumSkillPoints)
				.append("ammunition", ammunition)
				.append("arcaneBackgrounds", arcaneBackgrounds)
				.append("beasts", beasts)
				.append("edges", edges)
				.append("gear", gear)
				.append("gearCategories", gearCategories)
				.append("gearTypes", gearTypes)
				.append("hindrances", hindrances)
				.append("locations", locations)
				.append("personas", personas)
				.append("powers", powers)
				.append("races", races)
				.append("settingRules", settingRules)
				.append("skills", skills)
				.append("stories", stories)
				.append("vehicles", vehicles)
				.append("vehicleTypes", vehicleTypes)
				.toString();
	}

	public long getId() {

		return id;
	}

	public void setId(final long id) {
		this.id = id;
	}

	public long getVersion() {
		return version;
	}

	public void setVersion(final long version) {
		this.version = version;
	}

	public Set<Location> getLocations() {
		return locations;
	}

	public void setLocations(final Set<Location> locations) {
		this.locations = locations;
	}

	public Set<Skill> getSkills() {
		return skills;
	}

	public void setSkills(final Set<Skill> skills) {
		this.skills = skills;
	}

	public Set<Hindrance> getHindrances() {
		return hindrances;
	}

	public void setHindrances(final Set<Hindrance> hindrances) {
		this.hindrances = hindrances;
	}

	public Set<Edge> getEdges() {
		return edges;
	}

	public void setEdges(final Set<Edge> edges) {
		this.edges = edges;
	}

	public Set<Gear> getGear() {
		return gear;
	}

	public void setGear(final Set<Gear> gear) {
		this.gear = gear;
	}

	public Set<Vehicle> getVehicles() {
		return vehicles;
	}

	public void setVehicles(final Set<Vehicle> vehicles) {
		this.vehicles = vehicles;
	}

	public Set<SettingRule> getSettingRules() {
		return settingRules;
	}

	public void setSettingRules(final Set<SettingRule> settingRules) {
		this.settingRules = settingRules;
	}

	public Set<ArcaneBackground> getArcaneBackgrounds() {
		return arcaneBackgrounds;
	}

	public void setArcaneBackgrounds(final Set<ArcaneBackground> arcaneBackgrounds) {
		this.arcaneBackgrounds = arcaneBackgrounds;
	}

	public Set<Power> getPowers() {
		return powers;
	}

	public void setPowers(final Set<Power> powers) {
		this.powers = powers;
	}

	public Set<Persona> getPersonas() {
		return personas;
	}

	public void setPersonas(final Set<Persona> personas) {
		this.personas = personas;
	}

	public Set<Race> getRaces() {
		return races;
	}

	public void setRaces(final Set<Race> races) {
		this.races = races;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(final String description) {
		this.description = description;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}


}
