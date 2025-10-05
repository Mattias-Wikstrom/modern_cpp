// Version of the students example that uses the Registry/Mediator pattern; Conversion by ChatGPT (+ Claude.ai)
#include <iostream>
#include <vector>
#include <string>
#include <optional>
#include <algorithm>
#include <numeric>
#include <stdexcept>
#include <unordered_map>

namespace school {

// ------------------ Student ------------------
class Student {
public:
    Student(int id, std::string name) : id(id), name(std::move(name)) {}

    int getId() const noexcept { return id; }
    const std::string& getName() const noexcept { return name; }

private:
    int id;
    std::string name;
};

// ------------------ Course ------------------
class Course {
public:
    Course(int id, int hours = 3) : id(id), hours(hours) {
        if (hours <= 0) throw std::invalid_argument("Hours must be positive");
    }

    int getId() const noexcept { return id; }
    int getHours() const noexcept { return hours; }

private:
    int id;
    int hours;
};

// ------------------ Enrollment System (Registry / Mediator) ------------------
class EnrollmentSystem {
public:
    struct GradeRecord {
        int studentId;
        int courseId;
        float grade;
    };

    bool enroll(int studentId, int courseId) {
        auto it = std::find_if(grades.begin(), grades.end(),
            [studentId, courseId](const GradeRecord& r){
                return r.studentId == studentId && r.courseId == courseId;
            });
        if (it != grades.end()) return false; // already enrolled

        grades.push_back({studentId, courseId, 0.0f});
        return true;
    }

    void setGrade(int studentId, int courseId, float grade) {
        if (grade < 0.0f || grade > 4.0f)
            throw std::invalid_argument("Grade must be between 0.0 and 4.0");

        auto it = std::find_if(grades.begin(), grades.end(),
            [studentId, courseId](const GradeRecord& r){
                return r.studentId == studentId && r.courseId == courseId;
            });

        if (it != grades.end()) {
            it->grade = grade;
        }
    }

    std::optional<float> getGrade(int studentId, int courseId) const {
        auto it = std::find_if(grades.begin(), grades.end(),
            [studentId, courseId](const GradeRecord& r){
                return r.studentId == studentId && r.courseId == courseId;
            });
        return (it != grades.end()) ? std::optional{it->grade} : std::nullopt;
    }

    float calculateCourseAverage(int courseId) const {
        float sum = 0.0f;
        int count = 0;
        for (const auto& r : grades) {
            if (r.courseId == courseId) {
                sum += r.grade;
                ++count;
            }
        }
        return count > 0 ? (sum / count) : 0.0f;
    }

    float calculateGPA(int studentId, const std::vector<Course>& courses) const {
        float totalPoints = 0.0f;
        int totalHours = 0;

        for (const auto& course : courses) {
            auto grade = getGrade(studentId, course.getId());
            if (grade) {
                totalPoints += (*grade) * course.getHours();
                totalHours += course.getHours();
            }
        }

        return totalHours > 0 ? (totalPoints / totalHours) : 0.0f;
    }

    float curvedGrade(int studentId, int courseId) const {
        auto grade = getGrade(studentId, courseId);
        if (!grade) return 0.0f;

        float avg = calculateCourseAverage(courseId);
        if (avg < 0.01f) return 0.0f;

        float curved = 3.0f * (*grade) / avg;
        return std::min(curved, 4.0f);
    }

private:
    std::vector<GradeRecord> grades;
};

} // namespace school

// ------------------ main -------------------------
int main() {
    using namespace school;

    Student harry(1, "Harry");
    Student anne(2, "Anne");

    Course geo101(101);
    Course engl201(201, 4);

    EnrollmentSystem registry;

    registry.enroll(harry.getId(), geo101.getId());
    registry.enroll(anne.getId(), geo101.getId());
    registry.enroll(harry.getId(), engl201.getId());

    registry.setGrade(harry.getId(), geo101.getId(), 3.0f);
    registry.setGrade(anne.getId(), geo101.getId(), 2.5f);
    registry.setGrade(harry.getId(), engl201.getId(), 2.0f);

    std::vector<Course> courses = {geo101, engl201};

    std::cout << "Average geo101 grade   = " << registry.calculateCourseAverage(geo101.getId()) << "\n";
    std::cout << "Anne's grade           = " << registry.getGrade(anne.getId(), geo101.getId()).value_or(0.0f) << "\n";
    std::cout << "Anne's curved grade    = " << registry.curvedGrade(anne.getId(), geo101.getId()) << "\n";
    std::cout << "Harry's GPA            = " << registry.calculateGPA(harry.getId(), courses) << "\n";

    return 0;
}

