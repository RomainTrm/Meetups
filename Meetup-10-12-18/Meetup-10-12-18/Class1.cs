using NUnit.Framework;
using System;

namespace Meetup_10_12_18
{
    [TestFixture]
    public class Class1
    {
        [Test]
        public void ShouldBeVerboseWithVFlag()
        {
            var args = new[] { "-v" };
            var pattern = "vb";

            var result = Parse(args, pattern);
        }

        private object Parse(string[] args, string pattern)
        {
            throw new NotImplementedException();
        }
    }
}
